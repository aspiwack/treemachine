The tree machine
================

Arnaud Spiwack

Abstract
--------

A variant of Turing machines is introduced where the tape is replaced by a single tree which can be manipulated in a style akin to purely functional programming. This yields two benefits: first, the extra structure on the tape can be leveraged to write explicit constructions of machines much more easily than with Turing machines. Second, this new kind of machines models finely the asymptotic complexity of functional programming languages, and may allow to answer questions such as "is this problem inherently slower in functional languages".

Sources
-------

### Building instructions ###

To build the article, you need to have a Latex distribution installed, as well as Ocaml (version 3.12 or later) and the melt text processing tool. The command line to run is `ocamlbuild treemachine.pdf`. You can then find the pdf file in `_build/treemachine.pdf`

### License ###

The content of this repository is distributed under Creative Commons licence [CC BY 3.0](http://creativecommons.org/licenses/by/3.0/).
