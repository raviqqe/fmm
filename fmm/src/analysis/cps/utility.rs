use crate::{build, ir::Record, types::Type};

pub fn create_record(elements: &[(&str, &Type)]) -> Record {
    build::record(
        elements
            .iter()
            .map(|(name, type_)| build::variable(*name, (*type_).clone()))
            .collect(),
    )
}
