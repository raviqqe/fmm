use super::expression::Expression;
use crate::types;

#[derive(Clone, Debug, PartialEq)]
pub struct DeconstructUnion(Box<DeconstructUnionInner>);

#[derive(Clone, Debug, PartialEq)]
struct DeconstructUnionInner {
    type_: types::Union,
    union: Expression,
    member_index: usize,
    name: String,
}

impl DeconstructUnion {
    pub fn new(
        type_: types::Union,
        union: impl Into<Expression>,
        member_index: usize,
        name: impl Into<String>,
    ) -> Self {
        Self(
            DeconstructUnionInner {
                type_,
                union: union.into(),
                member_index,
                name: name.into(),
            }
            .into(),
        )
    }

    pub fn type_(&self) -> &types::Union {
        &self.0.type_
    }

    pub fn type_mut(&mut self) -> &mut types::Union {
        &mut self.0.type_
    }

    pub fn union(&self) -> &Expression {
        &self.0.union
    }

    pub fn union_mut(&mut self) -> &mut Expression {
        &mut self.0.union
    }

    pub fn member_index(&self) -> usize {
        self.0.member_index
    }

    pub fn name(&self) -> &str {
        &self.0.name
    }
}
