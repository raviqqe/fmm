# F--

[![GitHub Action](https://img.shields.io/github/workflow/status/raviqqe/fmm/test?style=flat-square)](https://github.com/raviqqe/fmm/actions?query=workflow%3Atest)
[![License](https://img.shields.io/github/license/raviqqe/fmm.svg?style=flat-square)](LICENSE)

The uncurried minimal functional programming language

It's designed to be a compiler target for high-level functional programming languages.

## Features

- [SSA](https://en.wikipedia.org/wiki/Static_single_assignment_form)
- Minimal
  - Only 4 control instructions
    - `branch`
    - `call`
    - `if`
    - `return`
  - No backward jump
- Portable
  - Pointer-sized integer
  - C-like union type
  - Boolean type
- Structural typing
- CPS transformation
- IR builder library

### Backends

- [LLVM](fmm-llvm)
- [C](fmm-c)
  - No guarantee for tail call optimization
  - No support for weak linkage

## Limitations

- Inductive types
- Weak typing
- No type inference

## Design notes

- Expressions have no side effect.
  - You can use them in constants.
- Instructions _can_ have side effects.

## License

[Apache 2.0](LICENSE)
