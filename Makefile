APP       := clojerl
REBAR3    := rebar3
ROOT_DIR  := ${CURDIR}
EBIN      ?= ${ROOT_DIR}/ebin
V         := @

.PHONY: all erlang clojure test shell clean

all: compile

compile: erlang clojure

test:
	@${REBAR3} as test do ct, cover, cover_result

shell:
	${V} ${REBAR3} as dev shell --sname clojerl-shell --setcookie clojerl

clean:
	${V} ${REBAR3} clean
	${V} rm -rf _build rebar.lock
	${V} rm -rf ${EBIN}
	${V} rm -rf priv/*.so c_src/*.o

repl: SHELL_OPTS = -sname clojerl-repl -setcookie clojerl -s clojerl
repl: SHELL_OPTS += -eval "'clojure.main':main([<<\"-r\">>])." -s clojerl start -noshell +pc unicode
repl: compile
	${V} rlwrap erl -pa `rebar3 path --ebin` ${CODE_PATH} ${SHELL_OPTS}

# ------------------------------------------------------------------------------
# Erlang compilation
# ------------------------------------------------------------------------------

APP_SRC       := ${CURDIR}/src
ERL_SRC       := ${CURDIR}/src/erl
C_SRC         := ${CURDIR}/c_src
PRIV          := ${CURDIR}/priv
INCLUDE       := ${CURDIR}/include
ERL_FILES     := $(shell find ${ERL_SRC} -name "*.erl")
COMPILE_FIRST := $(shell grep -rl "\-callback" ${ERL_SRC})
BEAM_FILES    := $(addprefix ${EBIN}/, $(notdir ${COMPILE_FIRST:.erl=.beam}) $(notdir ${ERL_FILES:.erl=.beam}))
ERL_OPTS      := +debug_info
ERLC          := erlc -o ${EBIN} -I ${INCLUDE} -pa ${EBIN} ${ERL_OPTS}

# Creates target for the beam file
define ERLC_TEMPLATE
${EBIN}/$(notdir ${1:.erl=.beam}): ${1}
	${V} mkdir -p ${EBIN}
	${V} ${ERLC} ${1}
	@echo Compiled $(1:${ERL_SRC}/%=%)
endef

$(foreach erl,${ERL_FILES},$(eval $(call ERLC_TEMPLATE,${erl})))

erlang: ${BEAM_FILES} ${EBIN}/${APP}.app ${PRIV}/${APP}.so

${PRIV}/${APP}.so:
	${V} make -f ${C_SRC}/Makefile -C ${C_SRC}

${EBIN}/${APP}.app: ${APP_SRC}/${APP}.app.src
	@echo ${APP}.app.src
	${V} cp ${APP_SRC}/${APP}.app.src ${EBIN}/${APP}.app

# ------------------------------------------------------------------------------
# Clojure compilation
# ------------------------------------------------------------------------------

CLJ_SRC     := ${ROOT_DIR}/src/clj
BOOT_SRC    := ${ROOT_DIR}/bootstrap
CLJ_TEST    := ${ROOT_DIR}/test/clj
CLJ_EXCLUDE := $(addprefix ${CLJ_SRC}/clojure/,core.clj core_deftype.clj core_print.clj)
CLJ_FILES   := $(filter-out ${CLJ_EXCLUDE},$(wildcard ${CLJ_SRC}/**/*.clj))

CODE_PATH   := ${EBIN} ${CLJ_SRC}
CLOJERLC    := bin/compile -o ${EBIN} -pa ${EBIN} -pa ${CLJ_SRC} -pa ${CLJ_TEST}

# Maps clj to target beam or ns: path/to/ns/file.clj -> ns.file[.ext]
define clj_to
$(subst .clj,${2},$(subst _,-,$(subst /,.,${1:${CLJ_SRC}/%=%})))
endef

clojure: clojure.core $(call clj_to,${CLJ_FILES},)

benchmark: all
	${V} ${CLOJERLC} ${CLJ_TEST}/benchmark/benchmark_runner.clj | \
	tee ${CLJ_TEST}/benchmark/result.txt

# This target is special since it is built from two sources erl and clj
${EBIN}/clojure.core.beam: ${BOOT_SRC}/clojure.core.erl ${CLJ_SRC}/clojure/core.clj
	${V} mkdir -p ${EBIN}
	${V} ${ERLC} ${BOOT_SRC}/clojure.core.erl
	${V} ${CLOJERLC} ${CLJ_SRC}/clojure/core.clj
	@ echo Compiled clojure.core

clojure.core: ${EBIN}/clojure.core.beam

# Creates target for the beam file
define COMPILE_CLJ_TEMPLATE
${EBIN}/$(call clj_to,${1},.beam): ${1}
	${V} ${CLOJERLC} ${1}
	@ echo Compiled $(call clj_to,${1},)
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
