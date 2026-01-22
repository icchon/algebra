use crate::engine::Engine;
use crate::parser::{
    ExprParser, integral::IntegralParser, linear_de::LinearDEParser, linear_e::LinearEParser,
    none::NotImplementedParser,
};

pub struct Session {
    pub parser: Box<dyn ExprParser>,
    pub engine: Engine,
}

impl Session {
    pub fn new(mode: &str) -> Result<Self, Box<dyn std::error::Error>> {
        let (parser, env_json): (Box<dyn ExprParser>, &str) = match mode {
            "linear" => (Box::new(LinearEParser), "./settings/linear_e.json"),
            "linear_de" => (Box::new(LinearDEParser), "./settings/linear_de.json"),
            "integral" => (Box::new(IntegralParser), "./settings/integral.json"),
            "none" => (Box::new(NotImplementedParser), "./settings/none.json"),
            _ => return Err(format!("Unknown mode: {}", mode).into()),
        };
        let engine = Engine::spawn(env_json)?;
        Ok(Session { parser, engine })
    }
}
