use crate::types;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Primitive {
    Boolean(bool),
    Float32(f32),
    Float64(f64),
    Integer8(u8),
    Integer32(u32),
    Integer64(u64),
    // Pointer integers are signed as it's expected to be architecture-agnostic
    // relative values.
    PointerInteger(i64),
}

impl Primitive {
    pub fn type_(&self) -> types::Primitive {
        match self {
            Primitive::Boolean(_) => types::Primitive::Boolean,
            Primitive::Float32(_) => types::Primitive::Float32,
            Primitive::Float64(_) => types::Primitive::Float64,
            Primitive::Integer8(_) => types::Primitive::Integer8,
            Primitive::Integer32(_) => types::Primitive::Integer32,
            Primitive::Integer64(_) => types::Primitive::Integer64,
            Primitive::PointerInteger(_) => types::Primitive::PointerInteger,
        }
    }
}

impl From<bool> for Primitive {
    fn from(boolean: bool) -> Self {
        Self::Boolean(boolean)
    }
}

impl From<f32> for Primitive {
    fn from(number: f32) -> Self {
        Self::Float32(number)
    }
}

impl From<f64> for Primitive {
    fn from(number: f64) -> Self {
        Self::Float64(number)
    }
}

impl From<i64> for Primitive {
    fn from(number: i64) -> Self {
        Self::PointerInteger(number)
    }
}
