# AOC 2018 - OCaml

Run with ocaml 4.11.1

These solutions are written as unix scripts - they generally work like
```shell
./dayXX.ml < input.txt > solution.txt
```
They _should_ run without additional packages or dependencies.

To compile to native code (after commenting out the shebang):
    ocamlbuild dayXX.native
or
    ocamlopt dayXX.ml -o dayXX
