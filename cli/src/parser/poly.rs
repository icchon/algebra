use super::ExprParser;
use lazy_static::lazy_static;
use pest::iterators::{Pair, Pairs};
use pest::prec_climber::{Assoc, Operator, PrecClimber};
use pest::Parser;
use pest_derive::Parser;
use serde_json::{json, Value};
use std::collections::HashMap;

#[derive(Parser)]
#[grammar = "BNF/poly.pest"]
pub struct PolyParser;

lazy_static! {
    static ref PREC_CLIMBER: PrecClimber<Rule> = {
        use Assoc::*;
        use Rule::*;

        PrecClimber::new(vec![
            Operator::new(add, Left) | Operator::new(sub, Left),
            Operator::new(mul, Left) | Operator::new(div, Left),
        ])
    };
}

fn parse_coeff(pair: Pair<Rule>) -> (i32, i32) {
    let s = pair.as_str();
    if let Some(pos) = s.find('/') {
        let num = s[..pos].parse::<i32>().unwrap_or(0);
        let mut den = s[pos+1..].parse::<i32>().unwrap_or(1);
        if den == 0 { den = 1; }
        (num, den)
    } else {
        (s.parse::<i32>().unwrap_or(0), 1)
    }
}

fn parse_poly(pair: Pair<Rule>) -> Value {
    let mut coeffs: HashMap<i32, (i32, i32)> = HashMap::new();
    let mut current_sign = 1;

    for inner_pair in pair.into_inner() {
        match inner_pair.as_rule() {
            Rule::sign => {
                if inner_pair.as_str() == "-" {
                    current_sign = -1;
                }
            }
            Rule::term => {
                let mut coeff = (1, 1);
                let mut power = 0;
                let mut has_var = false;

                for term_part in inner_pair.into_inner() {
                    match term_part.as_rule() {
                        Rule::coeff => {
                            coeff = parse_coeff(term_part);
                        }
                        Rule::power => {
                            has_var = true;
                            power = 1;
                            for power_part in term_part.into_inner() {
                                if power_part.as_rule() == Rule::number {
                                    power = power_part.as_str().parse::<i32>().unwrap();
                                }
                            }
                        }
                        _ => (),
                    }
                }

                if !has_var {
                    power = 0;
                }

                let (num, den) = coeff;
                let entry = coeffs.entry(power).or_insert((0, 1));
                
                // Add to existing coefficient, handling different denominators
                entry.0 = entry.0 * den + num * current_sign * entry.1;
                entry.1 = entry.1 * den;
                
                // Simplify fraction (not strictly necessary but good practice)
                let common = gcd(entry.0, entry.1);
                entry.0 /= common;
                entry.1 /= common;

                current_sign = 1; // Reset sign for next term
            }
            Rule::add => {
                current_sign = 1;
            }
            Rule::sub => {
                current_sign = -1;
            }
            _ => (),
        }
    }

    let max_power = coeffs.keys().max().copied().unwrap_or(0);
    let mut value_arr: Vec<Value> = vec![];
    for i in 0..=max_power {
        let (num, den) = coeffs.get(&i).copied().unwrap_or((0, 1));
        value_arr.push(json!([num, den]));
    }

    json!({
        "type": "polynomial",
        "value": value_arr
    })
}

// Function to compute GCD for simplifying fractions
fn gcd(a: i32, b: i32) -> i32 {
    if b == 0 {
        a.abs()
    } else {
        gcd(b, a % b)
    }
}


fn build_ast(pairs: Pairs<Rule>) -> Value {
    PREC_CLIMBER.climb(
        pairs,
        |pair: Pair<Rule>| match pair.as_rule() {
            Rule::primary => {
                let inner = pair.into_inner().next().unwrap();
                match inner.as_rule() {
                    Rule::binary_expr => build_ast(inner.into_inner()),
                    Rule::polynomial => parse_poly(inner),
                    r => unreachable!("primary can only contain binary_expr or polynomial but got {:?}", r),
                }
            }
            r => unreachable!("Climber should only pass primary pairs here. Got {:?}", r),
        },
        |lhs: Value, op: Pair<Rule>, rhs: Value| {
            let op_str = match op.as_rule() {
                Rule::add => "add",
                Rule::sub => "sub",
                Rule::mul => "mul",
                Rule::div => "div",
                _ => unreachable!(),
            };
            json!({
                "op": op_str,
                "lhs": lhs,
                "rhs": rhs,
            })
        },
    )
}

impl ExprParser for PolyParser {
    fn name(&self) -> &'static str {
        "poly"
    }

    fn parse(&self, source: &str) -> Result<String, String> {
        let expr_pair = <PolyParser as pest::Parser<Rule>>::parse(Rule::expr, source)
            .map_err(|e| e.to_string())?
            .next()
            .unwrap();

        let binary_expr_pair = expr_pair.into_inner().next().unwrap();
        let ast = build_ast(binary_expr_pair.into_inner());

        let final_json = json!({ "expression": ast });
        Ok(serde_json::to_string(&final_json).unwrap())
    }
}