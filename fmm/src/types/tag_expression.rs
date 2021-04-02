#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum TagExpression {
    PointerInteger(u64),
    Pointer(String),
}
