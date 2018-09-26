#!/bin/bash

rebar3 covertool generate
codecov -f _build/test/covertool/bookish_spork.covertool.xml
