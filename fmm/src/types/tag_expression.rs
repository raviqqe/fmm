#[derive(Clone, Debug, Hash, PartialEq)]
pub enum TagExpression {
    Boolean(bool),
    Integer8(u8),
    Integer32(u32),
    Integer64(u64),
    PointerInteger(i64),
    Pointer(String),
}
