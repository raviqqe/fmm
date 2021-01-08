# F--

The functional variant of C--

## Features

- [SSA](https://en.wikipedia.org/wiki/Static_single_assignment_form)
- Minimal
  - Only 4 control instructions
    - `branch`
    - `call`
    - `if`
    - `return`
  - No backward jump
- Fully-independent from backend
  - Pointer-sized integer
  - C-like union type
  - Bool type
- Structural typing

## Requirements for backend

- Tail call
- Heap allocation
- Atomic memory instructions
  - Or something equivalent if threading is not supported

## License

[MIT](LICENSE)
