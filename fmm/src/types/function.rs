use super::{calling_convention::CallingConvention, type_::Type};
use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
    rc::Rc,
};

#[derive(Clone, Debug, Eq, Ord, PartialOrd)]
pub struct Function(Rc<FunctionInner>);

#[derive(Debug, Eq, Ord, PartialEq, PartialOrd)]
struct FunctionInner {
    arguments: Vec<Type>,
    result: Rc<Type>,
    calling_convention: CallingConvention,
    hash: u64, // cached hash
}

impl Function {
    pub fn new(
        arguments: Vec<Type>,
        result: impl Into<Type>,
        calling_convention: CallingConvention,
    ) -> Self {
        let result = result.into();

        let mut hasher = DefaultHasher::new();

        arguments.hash(&mut hasher);
        result.hash(&mut hasher);
        calling_convention.hash(&mut hasher);

        Self(
            FunctionInner {
                arguments,
                result: result.into(),
                calling_convention,
                hash: hasher.finish(),
            }
            .into(),
        )
    }

    pub fn arguments(&self) -> &[Type] {
        &self.0.arguments
    }

    pub fn result(&self) -> &Type {
        &self.0.result
    }

    pub fn calling_convention(&self) -> CallingConvention {
        self.0.calling_convention
    }
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        Rc::as_ptr(&self.0) == Rc::as_ptr(&other.0)
            || self.0.arguments == other.0.arguments
                && self.0.result == other.0.result
                && self.0.calling_convention == other.0.calling_convention
    }
}

impl Hash for Function {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.0.hash.hash(hasher);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::Primitive;

    mod equal {
        use super::*;

        #[test]
        fn argument() {
            let function = Function::new(
                vec![Primitive::PointerInteger.into()],
                Primitive::PointerInteger,
                CallingConvention::Source,
            );

            assert_eq!(&function, &function);
            assert_ne!(
                function,
                Function::new(vec![], Primitive::PointerInteger, CallingConvention::Source)
            );
        }

        #[test]
        fn result() {
            let function =
                Function::new(vec![], Primitive::PointerInteger, CallingConvention::Source);

            assert_eq!(&function, &function);
            assert_ne!(
                function,
                Function::new(vec![], Primitive::Float64, CallingConvention::Source)
            );
        }

        #[test]
        fn calling_convention() {
            let function =
                Function::new(vec![], Primitive::PointerInteger, CallingConvention::Source);

            assert_eq!(&function, &function);
            assert_ne!(
                function,
                Function::new(vec![], Primitive::PointerInteger, CallingConvention::Target)
            );
        }
    }

    mod hash {
        use super::*;

        fn hash(value: &impl Hash) -> u64 {
            let mut hasher = DefaultHasher::new();

            value.hash(&mut hasher);

            hasher.finish()
        }

        #[test]
        fn argument() {
            let function = Function::new(
                vec![Primitive::PointerInteger.into()],
                Primitive::PointerInteger,
                CallingConvention::Source,
            );

            assert_eq!(hash(&function), hash(&function));
            assert_ne!(
                hash(&function),
                hash(&Function::new(
                    vec![],
                    Primitive::PointerInteger,
                    CallingConvention::Source
                ))
            );
        }

        #[test]
        fn result() {
            let function =
                Function::new(vec![], Primitive::PointerInteger, CallingConvention::Source);

            assert_eq!(hash(&function), hash(&function));
            assert_ne!(
                hash(&function),
                hash(&Function::new(
                    vec![],
                    Primitive::Float64,
                    CallingConvention::Source
                ))
            );
        }

        #[test]
        fn calling_convention() {
            let function =
                Function::new(vec![], Primitive::PointerInteger, CallingConvention::Source);

            assert_eq!(hash(&function), hash(&function));
            assert_ne!(
                hash(&function),
                hash(&Function::new(
                    vec![],
                    Primitive::PointerInteger,
                    CallingConvention::Target
                ))
            );
        }
    }
}
