.PHONY: doc edown codecov test
.DEFAULT_GOAL := test

COVERTOOL_REPORT=_build/test/covertool/bookish_spork.covertool.xml

doc: edown
	rebar3 edoc
	open doc/index.html

edown:
	rebar3 as edown edoc

codecov: $(COVERTOOL_REPORT)
	codecov -f COVERTOOL_REPORT

$(COVERTOOL_REPORT):
	rebar3 covertool generate

test:
	rebar3 do eunit, ct, dialyzer
