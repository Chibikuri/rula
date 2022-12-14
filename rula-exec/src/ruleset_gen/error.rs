#[derive(Debug)]
pub enum RuleSetGenError {
    InitializationError,
    RuleNameDuplicationError,
    SameNameExistInRuleError,
    NoTypeAnnotationError,
    NoRuleFoundError,
    UnknownTypeError,
    UnSendableFunctionError,
}
