use super::expression::Expression;

#[derive(Clone, Debug, PartialEq)]
pub struct MemoryCopy(Box<MemoryCopyInner>);

#[derive(Clone, Debug, PartialEq)]
struct MemoryCopyInner {
    source: Expression,
    destination: Expression,
    size: Expression,
}

impl MemoryCopy {
    pub fn new(
        source: impl Into<Expression>,
        destination: impl Into<Expression>,
        size: impl Into<Expression>,
    ) -> Self {
        Self(
            MemoryCopyInner {
                source: source.into(),
                destination: destination.into(),
                size: size.into(),
            }
            .into(),
        )
    }

    pub fn source(&self) -> &Expression {
        &self.0.source
    }

    pub fn source_mut(&mut self) -> &mut Expression {
        &mut self.0.source
    }

    pub fn destination(&self) -> &Expression {
        &self.0.destination
    }

    pub fn destination_mut(&mut self) -> &mut Expression {
        &mut self.0.destination
    }

    pub fn size(&self) -> &Expression {
        &self.0.size
    }

    pub fn size_mut(&mut self) -> &mut Expression {
        &mut self.0.size
    }
}
