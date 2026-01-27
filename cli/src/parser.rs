pub mod integral;
pub mod linear_de;
pub mod linear_e;
pub mod none;
pub mod quaternion;
pub mod poly;

pub trait ExprParser {
    fn parse(&self, source: &str) -> Result<String, String>;
    fn name(&self) -> &'static str;
}
