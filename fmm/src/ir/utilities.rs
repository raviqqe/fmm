use super::record::Record;
use crate::types::VOID_TYPE;
use once_cell::sync::Lazy;

thread_local! {
    static VOID_VALUE: Lazy<Record> = Lazy::new(|| Record::new(VOID_TYPE.clone(), vec![]));
}

pub fn void_value() -> Record {
    VOID_VALUE.with(|record| (&**record).clone())
}
