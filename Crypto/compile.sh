#!/usr/bin/env sh
ocamlfind ocamlopt prime.ml prime_naif.ml runtime.ml -package zarith -linkpkg -o test.exe
rm *.cm*
rm *.o
