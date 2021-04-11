use std::{
    error::Error,
    fmt::{self, Display, Formatter},
};

#[derive(Clone, Debug)]
pub enum BuildError {}

impl Display for BuildError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        write!(formatter, "{:?}", self)
    }
}

impl Error for BuildError {}
