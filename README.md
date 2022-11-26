# dabor

## What is dabor?
TODO: Description here
## Project Directory Structure
The project is separated into the following directories, managed by the [dune](https://dune.build/) build system:

- `bin`: executable compiler
- `doc`: documentations about dabor
- `lib`: the core library implementation - scanner, parser, ast, and more
- `test`: different test files

## How to Build??
First make sure that you have `dune` installed. Following [here](https://dune.readthedocs.io/en/stable/quick-start.html#install-dune) for guides on how to install.

From the project root:
 to compile, run
```
dune build
```

to execute the compiler, run
```
dune exec dabor
```

## Credit
Seoyoung Kweon:
- basic implementation of parser.mly, scanner.mll
- basic implementation of ast.ml, sast.ml
- implement (struct, vector) in parser.mly, ast.ml, sast.ml

Nicholas Greenspan:
- basic implementation of semant.ml, test.ml
- contributed to ast.ml and sast.ml

Annie Song:
- debugging on compilation error of basic parser.mly, scanner.mll, ast.ml
- general system (compilation code) for code testing
