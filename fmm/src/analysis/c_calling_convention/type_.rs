use super::context::Context;
use crate::{
    analysis::type_size,
    types::{self, void_type, Type},
};

pub fn transform_function(context: &Context, function: &types::Function) -> types::Function {
    if function.calling_convention() == types::CallingConvention::Target {
        let is_result_memory = is_memory_class(context, function.result());

        types::Function::new(
            function
                .arguments()
                .iter()
                .map(|type_| transform_type(context, type_))
                .chain(if is_result_memory {
                    Some(transform_type(context, function.result()).into())
                } else {
                    None
                })
                .collect(),
            if is_result_memory {
                void_type().into()
            } else {
                function.result().clone()
            },
            function.calling_convention(),
        )
    } else {
        function.clone()
    }
}

fn transform_type(context: &Context, type_: &Type) -> Type {
    if is_memory_class(context, type_) {
        types::Pointer::new(type_.clone()).into()
    } else {
        type_.clone()
    }
}

// The name, "memory class" comes from the C ABI on System V.
pub fn is_memory_class(context: &Context, type_: &Type) -> bool {
    match type_ {
        Type::Record(record) => {
            type_size::calculate_size(type_, context.word_bytes()) > 2 * context.word_bytes()
                || record
                    .fields()
                    .iter()
                    .any(|type_| is_memory_class(context, type_))
        }
        Type::Union(union) => union
            .members()
            .iter()
            .any(|type_| is_memory_class(context, type_)),
        Type::Function(_) | Type::Pointer(_) | Type::Primitive(_) => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const WORD_BYTES: usize = 8;

    mod function {
        use super::*;
        use pretty_assertions::assert_eq;

        #[test]
        fn transform_function_of_source_calling_convention() {
            let record = types::Record::new(vec![
                types::Primitive::Integer64.into(),
                types::Primitive::Integer64.into(),
                types::Primitive::Integer64.into(),
            ]);
            let function =
                types::Function::new(vec![], record.clone(), types::CallingConvention::Source);

            assert_eq!(
                transform_function(&Context::new(WORD_BYTES), &function),
                function
            );
        }

        #[test]
        fn transform_compatible_function() {
            let function = types::Function::new(
                vec![],
                types::Primitive::Integer64,
                types::CallingConvention::Target,
            );

            assert_eq!(
                transform_function(&Context::new(WORD_BYTES), &function),
                function
            );
        }

        #[test]
        fn transform_function_argument() {
            let record = types::Record::new(vec![
                types::Primitive::Integer64.into(),
                types::Primitive::Integer64.into(),
                types::Primitive::Integer64.into(),
            ]);
            let function = types::Function::new(
                vec![record.clone().into()],
                void_type(),
                types::CallingConvention::Target,
            );

            assert_eq!(
                transform_function(&Context::new(WORD_BYTES), &function),
                types::Function::new(
                    vec![types::Pointer::new(record).into()],
                    void_type(),
                    types::CallingConvention::Target
                )
            );
        }

        #[test]
        fn transform_function_result() {
            let record = types::Record::new(vec![
                types::Primitive::Integer64.into(),
                types::Primitive::Integer64.into(),
                types::Primitive::Integer64.into(),
            ]);
            let function =
                types::Function::new(vec![], record.clone(), types::CallingConvention::Target);

            assert_eq!(
                transform_function(&Context::new(WORD_BYTES), &function),
                types::Function::new(
                    vec![types::Pointer::new(record).into()],
                    void_type(),
                    types::CallingConvention::Target
                )
            );
        }
    }

    mod memory_class {
        use super::*;

        #[test]
        fn function() {
            assert!(!is_memory_class(
                &Context::new(WORD_BYTES),
                &types::Function::new(
                    vec![],
                    types::Primitive::PointerInteger,
                    types::CallingConvention::Target
                )
                .into()
            ));
        }

        #[test]
        fn primitive() {
            assert!(!is_memory_class(
                &Context::new(WORD_BYTES),
                &types::Primitive::PointerInteger.into()
            ));
        }

        #[test]
        fn pointer() {
            assert!(!is_memory_class(
                &Context::new(WORD_BYTES),
                &types::Pointer::new(types::Primitive::PointerInteger).into()
            ));
        }

        #[test]
        fn empty_record() {
            assert!(!is_memory_class(
                &Context::new(WORD_BYTES),
                &types::Record::new(vec![]).into()
            ));
        }

        #[test]
        fn small_record() {
            assert!(!is_memory_class(
                &Context::new(WORD_BYTES),
                &types::Record::new(vec![
                    types::Primitive::Integer64.into(),
                    types::Primitive::Integer64.into()
                ])
                .into()
            ));
        }

        #[test]
        fn large_record() {
            assert!(is_memory_class(
                &Context::new(WORD_BYTES),
                &types::Record::new(vec![
                    types::Primitive::Integer64.into(),
                    types::Primitive::Integer64.into(),
                    types::Primitive::Integer64.into()
                ])
                .into()
            ));
        }
    }
}
