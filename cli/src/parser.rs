pub mod linear_e;
pub mod none;

pub trait ExprParser {
    fn parse(&self, source: &str) -> Result<String, String>;
    fn name(&self) -> &'static str;
}
