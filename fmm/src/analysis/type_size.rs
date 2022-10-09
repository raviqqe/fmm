use crate::types::{self, Type};

/// Returns a size of a type in bytes.
///
/// Based on: https://refspecs.linuxfoundation.org/elf/x86_64-SysV-psABI.pdf
pub fn calculate_size(type_: &Type, word_bytes: usize) -> usize {
    match type_ {
        Type::Primitive(primitive) => match primitive {
            types::Primitive::Boolean | types::Primitive::Integer8 => 1,
            types::Primitive::Float32 | types::Primitive::Integer32 => 4,
            types::Primitive::Float64 | types::Primitive::Integer64 => 8,
            types::Primitive::PointerInteger => word_bytes,
        },
        Type::Record(record) => {
            let mut size = 0;

            for field in record.fields() {
                let alignment = calculate_alignment(field, word_bytes);

                size = size.max((size as f64 / alignment as f64).ceil() as usize * alignment);

                size += calculate_size(field, word_bytes);
            }

            size
        }
        Type::Union(union) => union
            .members()
            .iter()
            .map(|type_| calculate_size(type_, word_bytes))
            .max()
            .unwrap_or_default(),
        Type::Function(_) | Type::Pointer(_) => word_bytes,
    }
}

/// Returns an alignment of a type in bytes.
pub fn calculate_alignment(type_: &Type, word_bytes: usize) -> usize {
    match type_ {
        Type::Record(record) => record
            .fields()
            .iter()
            .map(|type_| calculate_alignment(type_, word_bytes))
            .max()
            .unwrap_or_default(),
        Type::Union(union) => union
            .members()
            .iter()
            .map(|type_| calculate_alignment(type_, word_bytes))
            .max()
            .unwrap_or_default(),
        Type::Function(_) | Type::Pointer(_) | Type::Primitive(_) => {
            calculate_size(type_, word_bytes)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod size {
        use super::*;

        #[test]
        fn primitives() {
            for (type_, size) in [
                (types::Primitive::Boolean, 1),
                (types::Primitive::Integer8, 1),
                (types::Primitive::Float32, 4),
                (types::Primitive::Integer32, 4),
                (types::Primitive::Float64, 8),
                (types::Primitive::Integer64, 8),
            ] {
                assert_eq!(calculate_size(&type_.into(), 8), size);
            }
        }

        #[test]
        fn pointer_integer() {
            assert_eq!(
                calculate_size(&types::Primitive::PointerInteger.into(), 4),
                4
            );
            assert_eq!(
                calculate_size(&types::Primitive::PointerInteger.into(), 8),
                8
            );
        }

        #[test]
        fn empty_record() {
            assert_eq!(calculate_size(&types::Record::new(vec![]).into(), 8), 0);
        }

        #[test]
        fn record_with_field() {
            assert_eq!(
                calculate_size(
                    &types::Record::new(vec![types::Primitive::Integer8.into()]).into(),
                    8
                ),
                1
            );
        }

        #[test]
        fn record_with_fields() {
            assert_eq!(
                calculate_size(
                    &types::Record::new(vec![
                        types::Primitive::Integer8.into(),
                        types::Primitive::Integer8.into()
                    ])
                    .into(),
                    8
                ),
                2
            );
        }

        #[test]
        fn record_with_aligned_4_byte_field() {
            assert_eq!(
                calculate_size(
                    &types::Record::new(vec![
                        types::Primitive::Integer8.into(),
                        types::Primitive::Integer32.into()
                    ])
                    .into(),
                    8
                ),
                8
            );
        }

        #[test]
        fn record_with_aligned_8_byte_field() {
            assert_eq!(
                calculate_size(
                    &types::Record::new(vec![
                        types::Primitive::Integer8.into(),
                        types::Primitive::Integer64.into()
                    ])
                    .into(),
                    8
                ),
                16
            );
        }

        #[test]
        fn record_with_4_byte_fields() {
            assert_eq!(
                calculate_size(
                    &types::Record::new(vec![
                        types::Primitive::Integer8.into(),
                        types::Primitive::Integer8.into(),
                        types::Primitive::Integer8.into(),
                        types::Primitive::Integer8.into()
                    ])
                    .into(),
                    8
                ),
                4
            );
        }

        #[test]
        fn record_with_5_byte_fields() {
            assert_eq!(
                calculate_size(
                    &types::Record::new(vec![
                        types::Primitive::Integer8.into(),
                        types::Primitive::Integer8.into(),
                        types::Primitive::Integer8.into(),
                        types::Primitive::Integer8.into(),
                        types::Primitive::Integer8.into()
                    ])
                    .into(),
                    8
                ),
                5
            );
        }

        #[test]
        fn record_with_4_byte_fields_and_integer_32_field() {
            assert_eq!(
                calculate_size(
                    &types::Record::new(vec![
                        types::Primitive::Integer8.into(),
                        types::Primitive::Integer8.into(),
                        types::Primitive::Integer8.into(),
                        types::Primitive::Integer8.into(),
                        types::Primitive::Integer32.into(),
                    ])
                    .into(),
                    8
                ),
                8
            );
        }

        #[test]
        fn nested_record() {
            assert_eq!(
                calculate_size(
                    &types::Record::new(vec![
                        types::Record::new(vec![
                            types::Primitive::Integer32.into(),
                            types::Primitive::Integer8.into(),
                        ])
                        .into(),
                        types::Primitive::Integer8.into()
                    ])
                    .into(),
                    8
                ),
                6
            );
        }

        #[test]
        fn nested_record_with_alignment() {
            assert_eq!(
                calculate_size(
                    &types::Record::new(vec![
                        types::Record::new(vec![
                            types::Primitive::Integer8.into(),
                            types::Primitive::Integer32.into(),
                        ])
                        .into(),
                        types::Primitive::Integer8.into()
                    ])
                    .into(),
                    8
                ),
                9
            );
        }
    }

    mod alignment {
        use super::*;

        #[test]
        fn primitives() {
            for (type_, size) in [
                (types::Primitive::Boolean, 1),
                (types::Primitive::Integer8, 1),
                (types::Primitive::Float32, 4),
                (types::Primitive::Integer32, 4),
                (types::Primitive::Float64, 8),
                (types::Primitive::Integer64, 8),
            ] {
                assert_eq!(calculate_alignment(&type_.into(), 8), size);
            }
        }

        #[test]
        fn pointer_integer() {
            assert_eq!(
                calculate_alignment(&types::Primitive::PointerInteger.into(), 4),
                4
            );
            assert_eq!(
                calculate_alignment(&types::Primitive::PointerInteger.into(), 8),
                8
            );
        }

        #[test]
        fn empty_record() {
            assert_eq!(
                calculate_alignment(&types::Record::new(vec![]).into(), 8),
                0
            );
        }

        #[test]
        fn record_with_field() {
            assert_eq!(
                calculate_alignment(
                    &types::Record::new(vec![types::Primitive::Integer8.into()]).into(),
                    8
                ),
                1
            );
        }

        #[test]
        fn record_with_fields() {
            assert_eq!(
                calculate_alignment(
                    &types::Record::new(vec![
                        types::Primitive::Integer8.into(),
                        types::Primitive::Integer8.into()
                    ])
                    .into(),
                    8
                ),
                1
            );
        }

        #[test]
        fn record_with_aligned_4_byte_field() {
            assert_eq!(
                calculate_alignment(
                    &types::Record::new(vec![
                        types::Primitive::Integer8.into(),
                        types::Primitive::Integer32.into()
                    ])
                    .into(),
                    8
                ),
                4
            );
        }

        #[test]
        fn record_with_aligned_8_byte_field() {
            assert_eq!(
                calculate_alignment(
                    &types::Record::new(vec![
                        types::Primitive::Integer8.into(),
                        types::Primitive::Integer64.into()
                    ])
                    .into(),
                    8
                ),
                8
            );
        }
    }
}
