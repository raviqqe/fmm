// TODO Split float and integer primitive types.
// Allow only integer types in bitwise operations.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Primitive {
    Boolean,
    Float32,
    Float64,
    Integer8,
    Integer32,
    Integer64,
    PointerInteger,
}
