# dabor

## What is dabor?
Our proposed language, Dabor, aims to offer programmers an intuitive and efficient means of creating board games. This language will provide flexibility to create not only a conventional board game, such as Checkers but also user-customized games with new rules.

## Project Directory Structure
The project is separated into the following directories, managed by the [dune](https://dune.build/) build system:

- `bin`: executable compiler
- `doc`: documentations about dabor
- `lib`: the core library implementation - scanner, parser, ast, and more
- `test`: different test files
- `pdabor`: initialized project

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

to run test files, run
```
dune runtest
```
- if dune runtest is outputing lli or llc error, update path of lli and llc according to the local machine path in test/testall.sh.
- if dune runtest encounters permission error, update test/testall.sh permission with `chmod +x test/testall.sh` command.
