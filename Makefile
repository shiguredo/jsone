.PHONY: all upgrade compile test proper dialyzer efmt-check fmt clean distclean publish

all: clean upgrade efmt-check compile dialyzer test

upgrade:
	@./rebar3 plugins upgrade --all
	@./rebar3 upgrade --all

compile:
	@./rebar3 xref

clean:
	@./rebar3 clean

test:
	@./rebar3 as test eunit
	@./rebar3 as test cover

proper: compile
	@./rebar3 as test proper

dialyzer:
	@./rebar3 dialyzer

efmt-check:
	@RUST_LOG=warn efmt --check --parallel --check-line-length 120

fmt:
	@efmt -w --parallel

distclean:
	@./rebar3 clean --all

publish:
	@./rebar3 hex publish package
