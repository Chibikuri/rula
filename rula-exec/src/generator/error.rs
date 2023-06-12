#[derive(Debug)]
pub enum CompileError{
    ParentScopeNotFoundError,
    ParentScopeDroppedError,
}