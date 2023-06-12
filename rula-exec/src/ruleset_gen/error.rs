#[derive(Debug)]
pub enum RuleSetGenError {
    InitializationError,
    RuleNameDuplicationError,
    SameNameExistInRuleError,
    NoTypeAnnotationError,
    NoRuleFoundError,
    UnknownTypeError,
    UnSendableFunctionError,
    ArgumentNumberError,
    NeedIdentifierTypeAnnotationError,
}

#[derive(Debug)]
pub enum InternalError {
    ParentScopeNotFoundError,
    ParentScopeDroppedError,
    NoSymbolFoundError,
}
