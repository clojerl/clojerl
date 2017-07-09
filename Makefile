.SILENT:

REBAR3    := rebar3
V         := @

EBIN      ?= ${CURDIR}/ebin
ERL_SRC   := ${CURDIR}/src/erl
INCLUDE   := ${CURDIR}/include
ERL_OPTS  := +debug_info
ERLC      := erlc -o ${EBIN} -I ${INCLUDE} -pa ${EBIN} ${ERL_OPTS}

.PHONY: all clojure test shell clean

all: compile

compile:
	${V} ${REBAR3} compile

test: clean
	${V} ${REBAR3} as test do ct, cover, cover_result

dialyzer: clean
	${V} NO_CLOJURE=1 ${REBAR3} dialyzer

shell:
	${V} ${REBAR3} as dev shell --sname clojerl-shell --setcookie clojerl

clean:
	${V} ${REBAR3} clean
	${V} rm -rf _build rebar.lock
	${V} rm -rf ${EBIN}
	${V} rm -rf priv/*.so c_src/*.o

travis-ci: test dialyzer

repl: SHELL_OPTS = -sname clojerl-repl -setcookie clojerl -s clojerl
repl: SHELL_OPTS += -eval "'clojure.main':main([<<\"-r\">>])." -s clojerl start -noshell +pc unicode
repl: compile
	${V} rlwrap erl -pa `rebar3 path --ebin` ${CODE_PATH} ${SHELL_OPTS}

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
CLJ_FILES      := $(filter-out ${CLJ_EXCLUDE},$(wildcard ${CLJ_SRC}/**/*${EXT}))

CODE_PATH   := ${EBIN} ${CLJ_SRC}
CLOJERLC    := bin/compile -o ${EBIN} -pa ${EBIN} -pa ${CLJ_SRC} -pa ${CLJ_TEST}

# Maps clj to target beam or ns: path/to/ns/some_file${EXT} -> ns.some-file[.ext]
define clj_to
$(subst ${EXT},${2},$(subst _,-,$(subst /,.,${1:${CLJ_SRC}/%=%})))
endef

clojure: clojure.core $(call clj_to,${CLJ_FILES},)

benchmark: all
	${V} ${CLOJERLC} --time -pa `rebar3 path --ebin` ${CLJ_TEST}/benchmark/benchmark_runner${EXT} | \
	tee ${CLJ_TEST}/benchmark/result.txt

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
