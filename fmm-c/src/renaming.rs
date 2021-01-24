use fmm::ir::*;

pub fn rename_names(module: &Module) -> Module {
    fmm::analysis::rename_names(module, |name| {
        if name == "" {
            "_".into()
        } else {
            name.chars()
                .map(|char| if char.is_alphanumeric() { char } else { '_' })
                .collect()
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rename_empty_name() {
        assert_eq!(
            rename_names(&Module::new(
                vec![VariableDeclaration::new(
                    "",
                    fmm::types::Primitive::PointerInteger
                )],
                vec![],
                vec![],
                vec![]
            )),
            Module::new(
                vec![VariableDeclaration::new(
                    "_",
                    fmm::types::Primitive::PointerInteger
                )],
                vec![],
                vec![],
                vec![]
            )
        );
    }

    #[test]
    fn rename_invalid_name() {
        assert_eq!(
            rename_names(&Module::new(
                vec![VariableDeclaration::new(
                    "$x",
                    fmm::types::Primitive::PointerInteger
                )],
                vec![],
                vec![],
                vec![]
            )),
            Module::new(
                vec![VariableDeclaration::new(
                    "_x",
                    fmm::types::Primitive::PointerInteger
                )],
                vec![],
                vec![],
                vec![]
            )
        );
    }
}
