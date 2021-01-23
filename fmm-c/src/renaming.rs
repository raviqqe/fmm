use fmm::ir::*;

pub fn rename_names(module: &Module) -> Module {
    fmm::analysis::rename_names(module, |name| {
        name.chars()
            .map(|char| if char.is_alphanumeric() { char } else { '_' })
            .collect()
    })
}
