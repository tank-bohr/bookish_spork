# How to contribute to Bookish spork

## How do I run an automated build?

`make all` or just `make` will run EUnit suite, CT suite, dialyzer and elvis

## Where should I fix documentation?

Bookish spork supports both html and markdown (via edown) formats for documentation. You shouldn't change README.md files in doc directory becouse it is generated from `doc/overview.edoc`. It is slightly different for markdown (it has badges embedded).

`make doc` will generate both markdown and html for [hexdocs](https://hexdocs.pm/bookish_spork/).
