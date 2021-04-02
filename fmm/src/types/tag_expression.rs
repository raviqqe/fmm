#[derive(Clone, Debug, Hash, PartialEq)]
pub enum TagExpression {
    PointerInteger(i64),
    Pointer(String),
}
