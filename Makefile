.PHONY: all upgrade compile test proper dialyzer efmt-check elint-check fmt clean distclean publish

all: clean upgrade efmt-check elint-check compile dialyzer test

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

# prek.toml 経由で efmt / elint を実行する
efmt-check:
	@prek run efmt-check --all-files

elint-check:
	@prek run elint --all-files

fmt:
	@prek run efmt --all-files

distclean:
	@./rebar3 clean --all

publish:
	@./rebar3 hex publish package
