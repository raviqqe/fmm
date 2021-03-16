# F--

[![GitHub Action](https://img.shields.io/github/workflow/status/raviqqe/fmm/test?style=flat-square)](https://github.com/raviqqe/fmm/actions?query=workflow%3Atest)
[![License](https://img.shields.io/github/license/raviqqe/fmm.svg?style=flat-square)](LICENSE)

The more functional variant of C programming language

It's designed to be a target language for high-level functional programming languages.

## Features

- [SSA](https://en.wikipedia.org/wiki/Static_single_assignment_form)
- Minimal
  - Only 4 control instructions
    - `branch`
    - `call`
    - `if`
    - `return`
  - No backward jump
- Portability
  - Pointer-sized integer
  - C-like union type
  - Boolean type
- Structural typing
- IR builder library

### Backends

- [C](fmm-c)
- [LLVM](fmm-llvm)

## Limitations

- Inductive types
- Weakly typed
- No type inference

## Requirements for backend

- Tail call
- Heap allocation
- Atomic memory instructions
  - Or something equivalent if threading is not supported

## License

[MIT](LICENSE)
