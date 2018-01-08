.SILENT:

REBAR3    := rebar3
RLWRAP    := $(shell type -p rlwrap &> /dev/null && echo rlwrap || echo)
V         := @
EXAMPLE   ?= *

EBIN      ?= ${CURDIR}/ebin
ifndef NO_CLOJURE
ifdef REBAR_DEPS_DIR
	EBIN = ${REBAR_DEPS_DIR}/clojerl/ebin
endif
endif

ERL_SRC   := ${CURDIR}/src/erl
INCLUDE   := ${CURDIR}/include
ERL_OPTS  := +debug_info
ERLC      := erlc -o ${EBIN} -I ${INCLUDE} -pa ${EBIN} ${ERL_OPTS}

.PHONY: all clojure test shell clean

all: compile

compile:
	${V} if [ -n "${NO_CLOJURE}" ]; then echo "Not compiling clojure files"; fi;
	${V} ${REBAR3} compile

compile-examples: compile
	${V} ${CLOJERLC} test/clj/examples/${EXAMPLE}.clje

test: test-ct

test-ct: clean
	${V} ${REBAR3} as test do ct, cover, cover_result

test-clj:
	${V} ${CLOJURE_MAIN} -m clojure.test-clojure.run-tests ${CLJ_TEST}/clojure/test_clojure/ ${CLJ_TEST}/

dialyzer: clean
	${V} NO_CLOJURE=1 ${REBAR3} dialyzer

shell:
	${V} ${REBAR3} as dev shell --sname clojerl-shell --setcookie clojerl

clean:
	${V} ${REBAR3} clean
	${V} rm -rf _build rebar.lock
	${V} rm -rf ${EBIN}
	${V} rm -rf priv/*.so c_src/*.o

ci: test dialyzer

repl: compile
	${V} ${RLWRAP} ${CLOJURE_MAIN} -r

# ------------------------------------------------------------------------------
# Clojure compilation
# ------------------------------------------------------------------------------

BOOT_SRC       := ${CURDIR}/bootstrap
CLJ_SRC        := ${CURDIR}/src/clj
CLJ_TEST       := ${CURDIR}/test/clj
EXT            := .clje

EXCLUDE_PPRINT := $(wildcard ${CLJ_SRC}/clojure/pprint/*${EXT})
EXCLUDE_CORE   := $(addprefix ${CLJ_SRC}/clojure/,core${EXT} core_deftype${EXT} core_print${EXT})
CLJ_EXCLUDE    := ${EXCLUDE_CORE} ${EXCLUDE_PPRINT}
CLJ_ALL_FILES  := $(shell find ${CLJ_SRC} -type f -name "*${EXT}")
CLJ_FILES      := $(filter-out ${CLJ_EXCLUDE}, ${CLJ_ALL_FILES})

CLOJERL      := bin/clojerl -pa ${CLJ_SRC} -pa ${CLJ_TEST} -pa ${EBIN}
CLOJERLC     := ${CLOJERL} --compile -o ${EBIN}
CLOJURE_MAIN := ${CLOJERL} --clojure.main

# Maps clj to target beam or ns: path/to/ns/some_file${EXT} -> ns.some-file[.ext]
define clj_to
$(subst ${EXT},${2},$(subst _,-,$(subst /,.,${1:${CLJ_SRC}/%=%})))
endef

clojure: clojure.core $(call clj_to,${CLJ_FILES},)

benchmark: all
	${V} cp ${CLJ_TEST}/benchmark/result.txt ${CLJ_TEST}/benchmark/result.prev.txt
	${V} (time ${CLOJURE_MAIN} -m benchmark.benchmark-runner) 2>&1 | tee ${CLJ_TEST}/benchmark/result.txt
	${V} ${CLOJURE_MAIN} -m benchmark.report ${CLJ_TEST}/benchmark/result.txt ${CLJ_TEST}/benchmark/result.prev.txt

# This target is special since it is built from two sources erl and clj
${EBIN}/clojure.core.beam: ${BOOT_SRC}/clojure.core.erl ${CLJ_SRC}/clojure/core${EXT}
	@ echo -n Compiling clojure.core...
	${V} mkdir -p ${EBIN}
	${V} ${ERLC} ${BOOT_SRC}/clojure.core.erl
	${V} ${CLOJERLC} ${CLJ_SRC}/clojure/core${EXT}
	@ echo done

clojure.core: ${EBIN}/clojure.core.beam

# Creates target for the beam file
define COMPILE_CLJ_TEMPLATE
${EBIN}/$(call clj_to,${1},.beam): ${1}
	@ echo -n Compiling $(call clj_to,${1},)...
	${V} ${CLOJERLC} ${1}
	@ echo done
endef

# Creates target with the ns name
define COMPILE_NS_TEMPLATE
$(call clj_to,${1},): ${EBIN}/$(call clj_to,${1},.beam)
endef

# Targets for beam files
$(foreach clj,${CLJ_FILES},$(eval $(call COMPILE_CLJ_TEMPLATE,${clj})))

# Phony targets with namespaces
CLJ_NAMESPACES = $(foreach clj,${CLJ_FILES},$(call clj_to,${clj},))

.PHONY: ${CLJ_NAMESPACES} clojure.core

$(foreach clj,${CLJ_FILES},$(eval $(call COMPILE_NS_TEMPLATE,${clj})))
