use super::{
    function::Function, pointer::Pointer, primitive::Primitive, record::Record, union::Union,
};

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Type {
    Function(Function),
    Record(Record),
    Primitive(Primitive),
    Pointer(Pointer),
    Union(Union),
}

impl Type {
    pub fn to_function(&self) -> Option<&Function> {
        if let Type::Function(function) = self {
            Some(function)
        } else {
            None
        }
    }

    pub fn to_record(&self) -> Option<&Record> {
        if let Type::Record(record) = self {
            Some(record)
        } else {
            None
        }
    }

    pub fn to_primitive(&self) -> Option<Primitive> {
        if let Type::Primitive(primitive) = self {
            Some(*primitive)
        } else {
            None
        }
    }

    pub fn to_pointer(&self) -> Option<&Pointer> {
        if let Type::Pointer(pointer) = self {
            Some(pointer)
        } else {
            None
        }
    }

    pub fn to_union(&self) -> Option<&Union> {
        if let Type::Union(union) = self {
            Some(union)
        } else {
            None
        }
    }
}

impl From<Function> for Type {
    fn from(function: Function) -> Self {
        Self::Function(function)
    }
}

impl From<Record> for Type {
    fn from(record: Record) -> Self {
        Self::Record(record)
    }
}

impl From<Primitive> for Type {
    fn from(primitive: Primitive) -> Self {
        Self::Primitive(primitive)
    }
}

impl From<Pointer> for Type {
    fn from(pointer: Pointer) -> Self {
        Self::Pointer(pointer)
    }
}

impl From<Union> for Type {
    fn from(union: Union) -> Self {
        Self::Union(union)
    }
}
