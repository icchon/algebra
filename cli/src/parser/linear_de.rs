extern crate pest;
extern crate pest_derive;

use pest::iterators::{Pair, Pairs};
use pest_derive::Parser;
use serde_json::json;

use super::ExprParser;

#[derive(Parser)]
#[grammar = "BNF/linear_de.pest"]
pub struct LinearDEParser;

fn parse_coeff(pair: Pair<Rule>) -> (i32, i32) {
    let mut numbers = pair.into_inner();
    let n = numbers.next().unwrap().as_str().parse::<i32>().unwrap();
    if let Some(d_pair) = numbers.next() {
        let d = d_pair.as_str().parse::<i32>().unwrap();
        (n, d)
    } else {
        (n, 1)
    }
}

fn parse_y_term(pair: Pair<Rule>) -> (usize, (i32, i32)) {
    let mut inner = pair.into_inner();
    let first = inner.next().unwrap();

    let coeff;
    let y_var_pair;

    if first.as_rule() == Rule::coeff {
        coeff = parse_coeff(first);
        y_var_pair = inner.next().unwrap();
    } else {
        coeff = (1, 1);
        y_var_pair = first;
    }

    let y_var_inner = y_var_pair.into_inner().next().unwrap();
    let degree = match y_var_inner.as_rule() {
        Rule::d0 => 0,
        Rule::d1 => 1,
        Rule::d2 => 2,
        Rule::d3 => 3,
        _ => 0, 
    };

    (degree, coeff)
}

// Processes the left-hand side y_expr
fn parse_y_expr(pairs: Pairs<Rule>) -> Vec<(i32, i32)> {
    let mut terms = std::collections::BTreeMap::new();
    let mut current_sign = 1;

    for pair in pairs {
        match pair.as_rule() {
            Rule::sign => {
                if pair.as_str() == "-" {
                    current_sign = -1;
                }
            }
            Rule::y_term => {
                let (degree, (n, _d)) = parse_y_term(pair);
                let (num, _den) = terms.entry(degree).or_insert((0, 1));
                *num += n * current_sign;
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

    let max_degree = terms.keys().max().copied().unwrap_or(0);
    let mut result = vec![(0, 1); max_degree + 1];
    for (deg, val) in terms {
        if deg < result.len() {
            result[deg] = val;
        }
    }
    result
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
                x_inner.next(); 
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

fn parse_x_expr(pairs: Pairs<Rule>) -> Vec<(i32, i32)> {
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
                let (degree, (n, _d)) = parse_x_term(pair);
                let (num, _den) = poly.entry(degree).or_insert((0, 1));
                *num += n * current_sign;
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
    let mut result = vec![(0, 1); max_degree + 1];
    for (deg, val) in poly {
        result[deg] = val;
    }

    result
}

fn parse_exponent(pair: Pair<Rule>) -> (i32, i32) {
    let mut inner = pair.into_inner();
    let mut sign = 1;
    let mut coeff = (1, 1);
    let mut has_x = false;

    let first = inner.next().unwrap();
    let term_pair = if first.as_rule() == Rule::sign {
        if first.as_str() == "-" {
            sign = -1;
        }
        inner.next().unwrap()
    } else {
        first
    };

    for p in term_pair.into_inner() {
        match p.as_rule() {
            Rule::coeff => coeff = parse_coeff(p),
            Rule::x => has_x = true,
            _ => (),
        }
    }

    if !has_x {
        return (0, 1);
    }

    (coeff.0 * sign, coeff.1)
}

fn parse_fx(fx_pair: Pair<Rule>) -> Vec<serde_json::Value> {
    let mut forcing_terms = vec![];

    let mut sign = 1;
    let mut poly = vec![(1, 1)];
    let mut alpha = (0, 1);

    let mut inner = fx_pair.into_inner(); 

    let first_child = inner.peek().unwrap();
    if first_child.as_rule() == Rule::sign {
        if inner.next().unwrap().as_str() == "-" {
            sign = -1;
        }
    }

    let actual_term_pair = inner.next().unwrap();

    let mut x_block_opt: Option<Pair<Rule>> = None;
    let mut exp_func_opt: Option<Pair<Rule>> = None;

    match actual_term_pair.as_rule() {
        Rule::x_block => {
            x_block_opt = Some(actual_term_pair);
            if let Some(next_pair) = inner.next() {
                if next_pair.as_rule() == Rule::exp_func {
                    exp_func_opt = Some(next_pair);
                }
            }
        }
        Rule::exp_func => {
            exp_func_opt = Some(actual_term_pair);
            if let Some(next_pair) = inner.next() {
                if next_pair.as_rule() == Rule::x_block {
                    x_block_opt = Some(next_pair);
                }
            }
        }
        Rule::x_expr => {
            poly = parse_x_expr(actual_term_pair.into_inner());
        }
        _ => (),
    }

    if let Some(exp_pair) = exp_func_opt {
        let exponent_pair = exp_pair.into_inner().next().unwrap();
        alpha = parse_exponent(exponent_pair);
    }

    if let Some(xb_pair) = x_block_opt {
        let inner_xb = xb_pair.into_inner().next().unwrap();
        if inner_xb.as_rule() == Rule::x_expr {
            poly = parse_x_expr(inner_xb.into_inner());
        } else {
            // x_term
            let (degree, coeff) = parse_x_term(inner_xb);
            poly = vec![(0, 1); degree + 1];
            poly[degree] = coeff;
        }
    }

    if let Some(first_coeff) = poly.iter_mut().find(|(n, _d)| *n != 0) {
        first_coeff.0 *= sign;
    } else if alpha != (0, 1) {
        alpha.0 *= sign;
    }

    let term = json!({
        "poly": poly,
        "alpha": alpha
    });
    forcing_terms.push(term);

    forcing_terms
}

impl ExprParser for LinearDEParser {
    fn name(&self) -> &'static str {
        "linear_de"
    }

    fn parse(&self, source: &str) -> Result<String, String> {
        let pairs = <LinearDEParser as pest::Parser<Rule>>::parse(Rule::formula, source)
            .map_err(|e| e.to_string())?;

        let mut y_coeffs = vec![];
        let mut forcing_terms = vec![];

        for pair in pairs {
            if let Rule::formula = pair.as_rule() {
                let mut inner = pair.into_inner();
                let y_expr = inner.next().unwrap();
                let fx = inner.next().unwrap();

                y_coeffs = parse_y_expr(y_expr.into_inner());
                forcing_terms = parse_fx(fx);
            }
        }

        let json_output = json!({
            "coeffs": y_coeffs,
            "forcing": forcing_terms,
        });

        Ok(json_output.to_string())
    }
}
