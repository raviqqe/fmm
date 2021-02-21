#[macro_use]
mod attempt;
mod error;
mod parsers;

use crate::ir::*;
use attempt::*;
use combine::Parser;
pub use error::ParseError;
use parsers::*;

pub fn parse_instructions(source: &str) -> Result<Vec<Instruction>, ParseError> {
    many(instruction())
        .parse(source)
        .map(|(module, _)| module)
        .map_err(|error| ParseError::new(&error))
}
