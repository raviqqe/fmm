use crate::{build, ir::Record, types::Type};

pub fn create_environment_record(environment: &[(&str, &Type)]) -> Record {
    build::record(
        environment
            .iter()
            .map(|(name, type_)| build::variable(*name, (*type_).clone()))
            .collect(),
    )
}
