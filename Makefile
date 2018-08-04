# Don't try to apply built-in rules
.SUFFIXES:
# Show less verbose output
.SILENT:

REBAR3    := rebar3
RLWRAP    := $(shell type -p rlwrap &> /dev/null && echo rlwrap || echo)
V         := @
EXAMPLE   ?= *

.PHONY: all clojure test shell clean

all: compile

compile:
	${V} ${REBAR3} compile

compile-examples: compile
	${V} ${CLOJERLC} test/clj/examples/${EXAMPLE}.clje

test: clean
	${V} ${REBAR3} as test do ct, clojerl test, cover, cover_result

dialyzer: clean
	${V} ${REBAR3} dialyzer

shell:
	${V} ${REBAR3} as dev shell --sname clojerl-shell --setcookie clojerl

clean:
	${V} ${REBAR3} clean
	${V} rm -rf _build rebar.lock

ci: test dialyzer

repl: compile
	${V} ${RLWRAP} ${CLOJERL} -r
