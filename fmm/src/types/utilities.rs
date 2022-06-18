use super::{pointer::Pointer, primitive::Primitive, type_::Type};
use crate::types;
use once_cell::sync::Lazy;

thread_local! {
    static GENERIC_POINTER_TYPE: Lazy<Type> =
        Lazy::new(|| Pointer::new(Primitive::Integer8).into());

    static VOID_TYPE: Lazy<types::Record> = Lazy::new(|| types::Record::new(vec![]));
}

pub fn generic_pointer_type() -> Type {
    GENERIC_POINTER_TYPE.with(|type_| (*type_).clone())
}

pub fn void_type() -> types::Record {
    VOID_TYPE.with(|record| (*record).clone())
}
