hgc-deploy
=========

A set of tools for establishing and running Mercury containers.

Dependencies
------------

Requires:

 - OCaml (using version 4.0.0)
 - OPAM
 - Findlib
 - [Jane Street Core](https://github.com/janestreet/core)

The easiest way to install all of these is using OPAM:

	opam init
	opam install ocamlfind core

Building
--------

To build, just execute

	ocamlbuild -use-ocamlfind hgc_deploy.native