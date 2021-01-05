use super::alternative::Alternative;
use super::default_alternative::DefaultAlternative;
use super::expression::Expression;

#[derive(Clone, Debug, PartialEq)]
pub struct Switch {
    condition: Expression,
    alternatives: Vec<Alternative>,
    default_alternative: DefaultAlternative,
}

impl Switch {
    pub fn new(
        condition: impl Into<Expression>,
        alternatives: Vec<Alternative>,
        default_alternative: DefaultAlternative,
    ) -> Self {
        Self {
            condition: condition.into(),
            alternatives,
            default_alternative,
        }
    }

    pub fn condition(&self) -> &Expression {
        &self.condition
    }

    pub fn alternatives(&self) -> &[Alternative] {
        &self.alternatives
    }

    pub fn default_alternative(&self) -> &DefaultAlternative {
        &self.default_alternative
    }
}
