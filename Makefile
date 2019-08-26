.PHONY: all doc edown codecov dialyzer test lint eep48 ex_doc gen-cert clean
.DEFAULT_GOAL := all

COVERTOOL_REPORT=_build/test/covertool/bookish_spork.covertool.xml
COVERAGE_REPORT=_build/test/cover/index.html

all: test dialyzer lint

test:
	rebar3 do eunit, ct

dialyzer:
	rebar3 dialyzer

lint:
	elvis rock --verbose

doc: edown ex_doc
	rebar3 edoc
	open doc/index.html

edown:
	rebar3 as edown edoc

eep48:
	rebar3 as eep48 eep48

ex_doc: eep48
	mix deps.get
	mix docs
	cp logo.svg docs

cover: test
	open $(COVERAGE_REPORT)

coverage: coveralls codecov

coveralls:
	rebar3 coveralls send

codecov: $(COVERTOOL_REPORT)
	codecov -f $(COVERTOOL_REPORT)

$(COVERTOOL_REPORT):
	rebar3 covertool generate

gen-cert:
	make -C priv/cert all

clean:
	rebar3 clean
