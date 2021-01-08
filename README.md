# F--

The functional variant of C--

It's designed to be a target language for high-level strict functional languages.

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
  - Bool type
- Structural typing

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
