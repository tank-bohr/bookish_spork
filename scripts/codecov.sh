#!/bin/bash

rebar3 covertool generate
cp _build/test/covertool/bookish_spork.covertool.xml ./cobertura.xml
codecov
