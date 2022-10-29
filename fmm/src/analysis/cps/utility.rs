use crate::{
    build,
    ir::Record,
    types::{self, Type},
};

pub fn create_record(elements: &[(&str, &Type)]) -> Record {
    build::record(
        elements
            .iter()
            .copied()
            .map(|(name, type_)| build::variable(name, type_.clone()))
            .collect(),
    )
}

pub fn create_record_type(elements: &[(&str, &Type)]) -> types::Record {
    types::Record::new(
        elements
            .iter()
            .copied()
            .map(|(_, type_)| type_.clone())
            .collect(),
    )
}
