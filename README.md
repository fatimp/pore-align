# pore-align
[![](https://img.shields.io/badge/docs-stable-blue.svg)](https://fatimp.github.io/pore-align)

**Pore-align** is a program for registration of 3D images of porous media
(e.g. soil, hence the name).

Registration is the process of bringing different images (e.g. taken from
different angles or having different contrast and/or resolution) of the same
sample into one coordinate space.

TODO: Add an image

## Dependencies:

* [https://github.com/fatimp/SIFT3D](SIFT3D) version 2.0+
* faiss[^1]
* OpenBLAS
* LMDB
* libtiff
* SBCL (Any other implementation of Common Lisp will no work)
* Qlot

[^1]: Принадлежит компании Meta, признанной в РФ экстремистской
    организацией. Осуждаем её и не поддерживаем!

## Installation

For installation, launch a shell, go to the directory containing this repository
and exec

~~~~
$ qlot install
$ qlot exec sbcl --dynamic-space-size 100gb
~~~~

and execute the following command in the opened REPL:

``` lisp
(asdf:make :pore-align)
```

The binary executable will be located in `src`.

## Documentation

Visit our website for the documentation. Alternatively you can build it locally
from the REPL:

``` lisp
(asdf:load-system :codex)
(codex:document :pore-align)
```
