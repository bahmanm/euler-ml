# Project Euler in OCaml #
Solutions to [Project Euler](https://projecteuler.net/) problems in [OCaml](http://ocaml.org/):

* [Oasis](http://oasis.forge.ocamlcore.org/) for building the project
* [Batteries](http://batteries.forge.ocamlcore.org/) as the stdlib
* [OUnit](http://ounit.forge.ocamlcore.org/) for testing

*NOTE: This is a work in progress --obviously.*

# How To Run Tests #
Provided that you have already [installed OPAM](https://opam.ocaml.org/doc/Install.html), you need to
install a couple of dependencies:

```
$ opam install oasis ounit batteries
$ cd euler-ml
$ oasis setup
$ ./configure --enable-tests && make test
```
