use super::{pointer::Pointer, primitive::Primitive, type_::Type};
use once_cell::sync::Lazy;

pub static GENERIC_POINTER_TYPE: Lazy<Type> =
    Lazy::new(|| Pointer::new(Primitive::Integer8).into());
