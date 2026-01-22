use super::ExprParser;
use pest_derive::Parser;
use serde_json::json;
use std::collections::HashMap;

#[derive(Parser)]
#[grammar = "BNF/linear.pest"]
pub struct LinearEParser;

impl ExprParser for LinearEParser {
    fn parse(&self, source: &str) -> Result<String, String> {
        parse_formula_to_json(source)
    }

    fn name(&self) -> &'static str {
        "linear"
    }
}

#[derive(Debug)]
enum LinearEParseError {
    DuplicateDegree,
}

fn parse_expr_to_vec(
    expr_pair: pest::iterators::Pair<Rule>,
) -> Result<Vec<[i32; 2]>, LinearEParseError> {
    let mut terms_map = HashMap::new();
    let mut max_degree = 0;
    let mut next_sign = 1;

    for pair in expr_pair.into_inner() {
        match pair.as_rule() {
            Rule::sign | Rule::op => {
                next_sign = if pair.as_str() == "-" { -1 } else { 1 };
            }
            Rule::term => {
                let (degree, mut coeff) = extract_term_data(pair);
                coeff[0] *= next_sign;

                if terms_map.contains_key(&degree) {
                    return Err(LinearEParseError::DuplicateDegree);
                }

                terms_map.insert(degree, coeff);
                if degree > max_degree {
                    max_degree = degree;
                }
                next_sign = 1;
            }
            _ => {}
        }
    }

    let vec = (0..=max_degree)
        .map(|d| *terms_map.get(&d).unwrap_or(&[0, 1]))
        .collect();
    Ok(vec)
}

fn extract_term_data(pair: pest::iterators::Pair<Rule>) -> (usize, [i32; 2]) {
    let mut degree = 0;
    let mut coeff = [1, 1];

    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::coeff => {
                let mut c_inner = inner.into_inner();
                let num = c_inner.next().unwrap().as_str().parse().unwrap();
                let den = c_inner
                    .next()
                    .map(|n| n.as_str().parse().unwrap())
                    .unwrap_or(1);
                coeff = [num, den];
            }
            Rule::power => {
                let mut p_inner = inner.into_inner();
                p_inner.next();
                degree = p_inner
                    .next()
                    .map(|n| n.as_str().parse().unwrap())
                    .unwrap_or(1);
            }
            _ => {}
        }
    }
    (degree, coeff)
}

fn parse_formula_to_json(input: &str) -> Result<String, String> {
    let pairs = <LinearEParser as pest::Parser<Rule>>::parse(Rule::formula, input)
        .map_err(|e| format!("Syntax Error: {}", e))?;

    let formula_pair = pairs.into_iter().next().unwrap();
    let mut inner = formula_pair.into_inner();

    let left_pair = inner.next().unwrap();
    let right_pair = inner.next().unwrap();

    let left_vec = parse_expr_to_vec(left_pair).map_err(|e| format!("{:?}", e))?;
    let right_vec = parse_expr_to_vec(right_pair).map_err(|e| format!("{:?}", e))?;

    let out = json!({
        "left": left_vec,
        "right": right_vec
    });

    Ok(serde_json::to_string_pretty(&out).unwrap())
}
