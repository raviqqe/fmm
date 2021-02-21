use std::error::Error;
use std::fmt::Display;

#[derive(Debug, PartialEq)]
pub struct ParseError {
    message: String,
}

impl ParseError {
    pub fn new(error: &impl std::error::Error) -> Self {
        Self {
            message: format!("{}", error),
        }
    }
}

impl Error for ParseError {}

impl Display for ParseError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(formatter, "{}", &self.message)
    }
}
