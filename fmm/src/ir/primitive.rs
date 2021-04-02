use crate::types;
use std::hash::{Hash, Hasher};

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
            Self::Boolean(_) => types::Primitive::Boolean,
            Self::Float32(_) => types::Primitive::Float32,
            Self::Float64(_) => types::Primitive::Float64,
            Self::Integer8(_) => types::Primitive::Integer8,
            Self::Integer32(_) => types::Primitive::Integer32,
            Self::Integer64(_) => types::Primitive::Integer64,
            Self::PointerInteger(_) => types::Primitive::PointerInteger,
        }
    }
}

impl Eq for Primitive {}

impl Hash for Primitive {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        match self {
            Self::Boolean(value) => value.hash(hasher),
            Self::Float32(value) => format!("{}", value).hash(hasher),
            Self::Float64(value) => format!("{}", value).hash(hasher),
            Self::Integer8(value) => value.hash(hasher),
            Self::Integer32(value) => value.hash(hasher),
            Self::Integer64(value) => value.hash(hasher),
            Self::PointerInteger(value) => value.hash(hasher),
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
