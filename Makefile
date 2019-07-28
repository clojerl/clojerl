# Don't try to apply built-in rules
.SUFFIXES:
# Show less verbose output
.SILENT:

SHELL     := /bin/bash
REBAR3    := rebar3
RLWRAP    := $(shell type -p rlwrap &> /dev/null && echo rlwrap || echo)
V         := @
EXAMPLE   ?= *

.PHONY: all clojure test shell clean

all: compile

compile:
	${V} ${REBAR3} clojerl compile

test: clean
	${V} ${REBAR3} do ct, clojerl test

dialyzer: clean
	${V} ${REBAR3} dialyzer

shell:
	${V} ${REBAR3} as dev do clojerl compile, shell --sname clojerl-shell --setcookie clojerl

clean:
	${V} ${REBAR3} clean
	${V} rm -rf _build rebar.lock

ci: test dialyzer

repl: compile
	${V} ${RLWRAP} ${CLOJERL} -r

# ------------------------------------------------------------------------------
# Clojure Benchmarks
# ------------------------------------------------------------------------------

SCRIPTS  := ${CURDIR}/scripts
CLOJERL  := bin/clojerl -pa ${SCRIPTS}
EBIN     := ebin
CLOJERLC := ${CLOJERL} --compile -o ${EBIN}

benchmark: all
	${V} cp ${SCRIPTS}/benchmark/result.txt ${SCRIPTS}/benchmark/result.prev.txt
	${V} (time ${CLOJERL} -m benchmark.benchmark-runner) 2>&1 | tee ${SCRIPTS}/benchmark/result.txt
	${V} ${CLOJERL} -m benchmark.report ${SCRIPTS}/benchmark/result.txt ${SCRIPTS}/benchmark/result.prev.txt | tee ${SCRIPTS}/benchmark/report.md

CLJ_BENCH=${SCRIPTS}/benchmark/clojure.txt
CLJE_BENCH=${SCRIPTS}/benchmark/clojerl.txt
CLJ_VS_CLJE=${SCRIPTS}/benchmark/clojure-vs-clojerl.md

benchmark-comparison:
	${V} clj -i scripts/benchmark/benchmark_runner.cljc | tee ${CLJ_BENCH}
	${V} ${CLOJERL} -m benchmark.benchmark-runner | tee ${CLJE_BENCH}
	${V} ${CLOJERL} -m benchmark.compare ${CLJ_BENCH} ${CLJE_BENCH} | tee ${CLJ_VS_CLJE}
	${V} rm ${CLJ_BENCH} ${CLJE_BENCH}

compile-examples: compile
	${V} ${CLOJERLC} -vv ${SCRIPTS}/examples/${EXAMPLE}.clje
