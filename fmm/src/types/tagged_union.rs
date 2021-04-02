use super::primitive::Primitive;
use super::tag_expression::TagExpression;
use super::type_::Type;
use std::sync::Arc;

#[derive(Clone, Debug, Hash, PartialEq)]
pub struct TaggedUnion {
    tag_type: Primitive,
    members: Vec<TaggedUnionMember>,
}

impl TaggedUnion {
    pub const fn new(tag_type: Primitive, members: Vec<TaggedUnionMember>) -> Self {
        Self { tag_type, members }
    }

    pub fn tag_type(&self) -> Primitive {
        self.tag_type
    }

    pub fn members(&self) -> &[TaggedUnionMember] {
        &self.members
    }
}

#[derive(Clone, Debug, Hash, PartialEq)]
pub struct TaggedUnionMember {
    tag: TagExpression,
    payload: Arc<Type>,
}

impl TaggedUnionMember {
    pub fn new(tag: impl Into<TagExpression>, payload: impl Into<Type>) -> Self {
        Self {
            tag: tag.into(),
            payload: payload.into().into(),
        }
    }

    pub fn tag(&self) -> &TagExpression {
        &self.tag
    }

    pub fn payload(&self) -> &Type {
        &self.payload
    }
}
