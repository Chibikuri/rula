use std::fmt::Debug;

#[derive(Debug, PartialEq)]
pub enum RuLaCompileError {
    RuLaGenerationError,
    RuLaInitializationError(InitializationError),
    NoRuleFoundError,
    RuleDuplicationError,
}

#[derive(PartialEq)]
pub struct InitializationError {
    message: String,
}

impl InitializationError {
    pub fn new(message: &str) -> InitializationError {
        InitializationError {
            message: String::from(message),
        }
    }
}

impl Debug for InitializationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        format!("Value is not properly initialized {}", &self.message);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
}
