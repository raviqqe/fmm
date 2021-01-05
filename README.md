# F--

The functionnal variant of C--

## Features

- [SSA](https://en.wikipedia.org/wiki/Static_single_assignment_form)
- Minimal
  - Only two control instructions of `call` and `switch`
- Fully-independent from backend
  - Pointer-sized integer
  - C-like union type

## Requirements for backend

- Tail call
- Heap allocation
- Atomic memory instructions
  - Or something equivalent if threading is not supported

## License

[MIT](LICENSE)
