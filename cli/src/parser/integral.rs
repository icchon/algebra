use pest::iterators::{Pair, Pairs};
use pest_derive::Parser;
use serde_json::json;

use super::ExprParser;

#[derive(Parser)]
#[grammar = "BNF/integral.pest"]
pub struct IntegralParser;


fn parse_coeff(pair: Pair<Rule>) -> (i32, i32) {
    let mut numbers = pair.into_inner();
    let n = numbers.next().unwrap().as_str().parse::<i32>().unwrap();
    if let Some(d_pair) = numbers.next() {
        (n, d_pair.as_str().parse::<i32>().unwrap())
    } else {
        (n, 1)
    }
}

fn parse_x_term(pair: Pair<Rule>) -> (usize, (i32, i32)) {
    let mut coeff = (0, 1);
    let mut degree = 0;
    let mut has_coeff = false;
    let mut has_xp = false;

    for item in pair.into_inner() {
        match item.as_rule() {
            Rule::coeff => {
                coeff = parse_coeff(item);
                has_coeff = true;
            }
            Rule::x_p => {
                has_xp = true;
                let mut x_inner = item.into_inner();
                x_inner.next(); // "x"
                if let Some(pow) = x_inner.next() {
                    degree = pow.as_str().parse().unwrap();
                } else {
                    degree = 1;
                }
            }
            _ => (),
        }
    }
    if has_xp && !has_coeff {
        coeff = (1, 1);
    }
    (degree, coeff)
}

fn parse_x_expr(pairs: Pairs<Rule>) -> serde_json::Value {
    let mut poly = std::collections::BTreeMap::new();
    let mut current_sign = 1;

    for pair in pairs {
        match pair.as_rule() {
            Rule::sign => {
                if pair.as_str() == "-" {
                    current_sign = -1;
                }
            }
            Rule::x_term => {
                let (degree, (n, d)) = parse_x_term(pair);
                let (num, den) = poly.entry(degree).or_insert((0, 1));
                *num += n * current_sign;
                *den = d;
                current_sign = 1;
            }
            Rule::op => {
                if pair.as_str() == "-" {
                    current_sign = -1;
                } else {
                    current_sign = 1;
                }
            }
            _ => (),
        }
    }

    let max_degree = poly.keys().max().copied().unwrap_or(0);
    let mut result = vec![json!([0, 1]); max_degree + 1];
    for (deg, (n, d)) in poly {
        result[deg] = json!([n, d]);
    }
    json!(result)
}


fn parse_x_block_as_poly(pair: Pair<Rule>) -> serde_json::Value {
    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::x_expr => parse_x_expr(inner.into_inner()),
        Rule::x_term => {
            let (degree, (n, d)) = parse_x_term(inner);
            let mut coeffs = vec![json!([0, 1]); degree + 1];
            coeffs[degree] = json!([n, d]);
            json!(coeffs)
        }
        _ => unreachable!(),
    }
}

fn parse_frac_expr_as_rational_poly(pair: Pair<Rule>) -> serde_json::Value {
    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::fraction => {
            let mut fr_inner = inner.into_inner();
            let num_poly = parse_x_block_as_poly(fr_inner.next().unwrap());
            let den_poly = parse_x_block_as_poly(fr_inner.next().unwrap());
            json!({ "num": num_poly, "den": den_poly })
        }
        Rule::x_block => {
            let num_poly = parse_x_block_as_poly(inner);
            json!({ "num": num_poly, "den": json!([[1,1]]) })
        }
        _ => unreachable!(),
    }
}

fn parse_term(pair: Pair<Rule>, sign: i32) -> serde_json::Value {
    let mut frac_expr_opt: Option<Pair<Rule>> = None;
    let mut exp_func_opt: Option<Pair<Rule>> = None;
    let mut log_func_opt: Option<Pair<Rule>> = None;

    for p in pair.into_inner() {
        match p.as_rule() {
            Rule::frac_expr => frac_expr_opt = Some(p),
            Rule::exp_func => exp_func_opt = Some(p),
            Rule::log_func => log_func_opt = Some(p),
            _ => (),
        }
    }

    let mut frac_expr_part = frac_expr_opt.map_or(
        json!({ "num": [[1,1]], "den": [[1,1]] }),
        parse_frac_expr_as_rational_poly,
    );

    if let Some(num_coeffs) = frac_expr_part
        .get_mut("num")
        .and_then(|n| n.as_array_mut())
    {
        if let Some(first_elem) = num_coeffs
            .iter_mut()
            .find(|c| c.get(0).and_then(|v| v.as_i64()) != Some(0))
        {
            if let Some(n) = first_elem.get(0).and_then(|v| v.as_i64()) {
                if let Some(val) = first_elem.get_mut(0) {
                    *val = json!(n * sign as i64);
                }
            }
        } else if let Some(first_elem) = num_coeffs.get_mut(0) {
            if let Some(n) = first_elem.get(0).and_then(|v| v.as_i64()) {
                if let Some(val) = first_elem.get_mut(0) {
                    *val = json!(n * sign as i64);
                }
            }
        }
    }

    let mut term_json = serde_json::Map::new();
    term_json.insert("coeff".to_string(), frac_expr_part);

    if let Some(exp_pair) = exp_func_opt {
        let exponent = parse_x_expr(exp_pair.into_inner().next().unwrap().into_inner());
        term_json.insert("exponent".to_string(), exponent);
    }

    if let Some(log_pair) = log_func_opt {
        let argument = parse_x_expr(log_pair.into_inner().next().unwrap().into_inner());
        term_json.insert("log".to_string(), argument);
    }

    json!(term_json)
}

impl ExprParser for IntegralParser {
    fn name(&self) -> &'static str {
        "integral"
    }

    fn parse(&self, source: &str) -> Result<String, String> {
        let formula = <IntegralParser as pest::Parser<Rule>>::parse(Rule::formula, source)
            .map_err(|e| e.to_string())?
            .next()
            .unwrap();

        let mut terms = vec![];
        let mut current_sign = 1;

        for pair in formula.into_inner().next().unwrap().into_inner() {
            match pair.as_rule() {
                Rule::sign => {
                    if pair.as_str() == "-" {
                        current_sign = -1;
                    }
                }
                Rule::term => {
                    let term_json = parse_term(pair, current_sign);
                    terms.push(term_json);
                    current_sign = 1;
                }
                Rule::op => {
                    if pair.as_str() == "-" {
                        current_sign = -1;
                    } else {
                        current_sign = 1;
                    }
                }
                _ => (),
            }
        }

        let json_output = json!({ "terms": terms });
        Ok(json_output.to_string())
    }
}
