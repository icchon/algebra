use super::ExprParser;
pub struct NotImplementedParser;

impl ExprParser for NotImplementedParser {
    fn parse(&self, _: &str) -> Result<String, String> {
        Err("This parser is not yet implemented.".into())
    }
    fn name(&self) -> &'static str {
        "none"
    }
}
