use super::ExprParser;
use lazy_static::lazy_static;
use pest::iterators::{Pair, Pairs};
use pest::prec_climber::{Assoc, Operator, PrecClimber};
use pest::Parser;
use pest_derive::Parser;
use serde_json::{json, Value};

#[derive(Parser)]
#[grammar = "BNF/quaternion.pest"]
pub struct QuaternionParser;

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

fn parse_quaternion_term(pair: Pair<Rule>) -> (i32, Option<&str>) {
    let mut num = 1;
    let mut base = None;
    for inner_pair in pair.into_inner() {
        match inner_pair.as_rule() {
            Rule::number => num = inner_pair.as_str().parse::<i32>().unwrap(),
            Rule::base => base = Some(inner_pair.as_str()),
            _ => unreachable!(),
        }
    }
    (num, base)
}

fn parse_quaternion(pair: Pair<Rule>) -> Value {
    let mut r = (0, 1);
    let mut i = (0, 1);
    let mut j = (0, 1);
    let mut k = (0, 1);

    let mut current_sign = 1;

    for inner_pair in pair.into_inner() {
        match inner_pair.as_rule() {
            Rule::sign => {
                if inner_pair.as_str() == "-" {
                    current_sign = -1;
                } else {
                    current_sign = 1;
                }
            }
            Rule::term => {
                let (num, base_opt) = parse_quaternion_term(inner_pair);
                let val = num * current_sign;

                match base_opt {
                    Some("i") => i.0 += val,
                    Some("j") => j.0 += val,
                    Some("k") => k.0 += val,
                    None => r.0 += val,
                    _ => unreachable!(),
                }
                current_sign = 1; // reset after applying
            }
            _ => (), // EOI, WHITESPACE
        }
    }

    json!({
        "type": "quaternion",
        "value": [[r.0, r.1], [i.0, i.1], [j.0, j.1], [k.0, k.1]]
    })
}

fn build_ast(pairs: Pairs<Rule>) -> Value {
    PREC_CLIMBER.climb(
        pairs,
        |pair: Pair<Rule>| match pair.as_rule() {
            Rule::primary => {
                let inner = pair.into_inner().next().unwrap();
                match inner.as_rule() {
                    Rule::binary_expr => build_ast(inner.into_inner()),
                    Rule::quaternion => parse_quaternion(inner),
                    _ => unreachable!("primary can only contain binary_expr or quaternion"),
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

impl ExprParser for QuaternionParser {
    fn name(&self) -> &'static str {
        "quat"
    }

    fn parse(&self, source: &str) -> Result<String, String> {
        let expr_pair = <QuaternionParser as pest::Parser<Rule>>::parse(Rule::expr, source)
            .map_err(|e| e.to_string())?
            .next()
            .unwrap();

        let binary_expr_pair = expr_pair.into_inner().next().unwrap();

        let ast = build_ast(binary_expr_pair.into_inner());

        let final_json = json!({ "expression": ast });
        Ok(serde_json::to_string(&final_json).unwrap())
    }
}
