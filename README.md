# PCL Compiler

Compiler project for Pascal-based language PCL using OCaml with ocamllex and Menhir and LLVM

[Language specification](http://courses.softlab.ntua.gr/compilers/2019a/pcl2019.pdf)

## Installation

### Cloning the  repo

Use `--recursive` flag when cloning in order to clone the [edsger-lib repo](https://github.com/abenetopoulos/edsger_lib) as well
```
git clone https://github.com/pettza/NTUA-Compiler --recursive
```

### Building the library

The library file is produced by running:

```
./lib.sh
```

`nasm` is needed to produce the library. It can be installed via `apt-get`

### OCaml and opam

For the OCaml language, [opam](https://opam.ocaml.org/) is needed, with a switch version >=4.11

From `opam` the following packages need to be installed:

* dune
* menhir
* llvm

### Additional dependencies

A version of `llvm-dev` matching the `opam` package needs to be installed. This can be done with `apt-get`. This might need to be done prior to the `opam` package

`Clang` is used for linking the library. If it was not installed with `llvm` it can be installed seperately via `apt-get`

### Compiling the compiler

The executable is produced by running:

```
dune build pcl.exe
```
