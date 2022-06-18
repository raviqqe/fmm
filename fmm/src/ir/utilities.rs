use super::record::Record;
use crate::types::void_type;
use once_cell::sync::Lazy;

thread_local! {
    static VOID_VALUE: Lazy<Record> = Lazy::new(|| Record::new(void_type(), vec![]));
}

pub fn void_value() -> Record {
    VOID_VALUE.with(|record| (&**record).clone())
}
