use super::{pointer::Pointer, primitive::Primitive, type_::Type};
use crate::types;
use once_cell::sync::Lazy;

pub static GENERIC_POINTER_TYPE: Lazy<Type> =
    Lazy::new(|| Pointer::new(Primitive::Integer8).into());

pub static VOID_TYPE: Lazy<types::Record> = Lazy::new(|| types::Record::new(vec![]));
