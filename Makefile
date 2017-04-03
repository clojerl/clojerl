APP         := clojerl
REBAR3      := rebar3
ROOT_DIR    := ${CURDIR}

EBIN        := ${ROOT_DIR}/ebin

.PHONY: all erlang clojure bootstrap clojure.core test shell clean

all: compile

compile: erlang clojure

test:
	@${REBAR3} as test do ct, cover, cover_result

shell:
	@${REBAR3} as dev shell --sname clojerl-shell --setcookie clojerl

clean:
	@${REBAR3} clean
	@rm -rf **/*.beam ${EBIN} rebar.lock

repl: SHELL_OPTS = -sname clojerl-repl -setcookie clojerl -s clojerl
repl: SHELL_OPTS += -eval "'clojure.main':main([<<\"-r\">>])." -s clojerl start -noshell +pc unicode
repl: clojure.core clojure.main
	@rlwrap erl -pa `rebar3 path --ebin` ${CODE_PATH} ${SHELL_OPTS}

# ------------------------------------------------------------------------------
# Erlang compilation
# ------------------------------------------------------------------------------

APP_SRC    := ${CURDIR}/src
ERL_SRC    := ${CURDIR}/src/erl
C_SRC      := ${CURDIR}/c_src
PRIV       := ${CURDIR}/priv
INCLUDE    := ${CURDIR}/include
ERL_FILES  := $(shell find ${ERL_SRC} -name "*.erl")
BEAM_FILES := $(addprefix ${EBIN}/,$(notdir ${ERL_FILES:.erl=.beam}))
ERL_OPTS   := +debug_info
ERLC       := erlc -o ${EBIN} -I ${INCLUDE} ${ERL_OPTS}

# Creates target for the beam file
define ERLC_TEMPLATE
${EBIN}/$(notdir ${1:.erl=.beam}): ${1}
	@echo $(1:${ERL_SRC}/%=%)
	@mkdir -p $(EBIN)
	@${ERLC} ${1}
endef

$(foreach erl,${ERL_FILES},$(eval $(call ERLC_TEMPLATE,${erl})))

erlang: ${BEAM_FILES} ${EBIN}/${APP}.app ${PRIV}/${APP}.so

${PRIV}/${APP}.so:
	@make -f ${C_SRC}/Makefile -C ${C_SRC}

${EBIN}/${APP}.app: ${APP_SRC}/${APP}.app.src
	@echo ${APP}.app.src
	@cp ${APP_SRC}/${APP}.app.src ${EBIN}/${APP}.app

# ------------------------------------------------------------------------------
# Clojure compilation
# ------------------------------------------------------------------------------

CLJ_SRC     := ${ROOT_DIR}/src/clj
CLJ_TEST    := ${ROOT_DIR}/test/clj
CLJ_EXCLUDE := $(addprefix ${CLJ_SRC}/clojure/,core.clj core_deftype.clj core_print.clj)
CLJ_FILES   := $(filter-out ${CLJ_EXCLUDE},$(wildcard ${CLJ_SRC}/**/*.clj))

CODE_PATH   := ${EBIN} ${CLJ_SRC}
COMPILE     := bin/compile -o ${EBIN} -pa ${EBIN} -pa ${CLJ_SRC} -pa ${CLJ_TEST}

# Maps clj to target beam or ns
# path/to/ns/file.clj -> ns.file[.ext]
define clj_to
$(subst .clj,${2},$(subst _,-,$(subst /,.,${1:${CLJ_SRC}/%=%})))
endef

# Creates target for the beam file
define COMPILE_CLJ_TEMPLATE
${EBIN}/$(call clj_to,${1},.beam): ${1}
	@echo Compiling $(1:${CLJ_SRC}/%=%)
	@${COMPILE} ${1}
endef

# Creates target with the ns name
define COMPILE_NS_TEMPLATE
$(call clj_to,${1},): ${EBIN}/$(call clj_to,${1},.beam)
endef

# Targets for beam files
$(foreach clj,${CLJ_FILES},$(eval $(call COMPILE_CLJ_TEMPLATE,${clj})))

# Phony targets with namespaces
CLJ_NAMESPACES = $(foreach clj,${CLJ_FILES},$(call clj_to,${clj},))
.PHONY: ${CLJ_NAMESPACES}
$(foreach clj,${CLJ_FILES},$(eval $(call COMPILE_NS_TEMPLATE,${clj})))

${CLJ_NAMESPACES}: clojure.core

clojure.core: ${CLJ_SRC}/clojure/core.clj
	@echo Compiling clojure/core.clj
	@${COMPILE} $<

# When bootstrapping we need to force recompilation of clojure.core
# based on the .clj source file.
bootstrap: erlang clojure

clojure: erlang $(call clj_to,${CLJ_FILES},)

benchmark: bootstrap
	@${COMPILE} ${CLJ_TEST}/benchmark/benchmark_runner.clj | \
	tee ${CLJ_TEST}/benchmark/result.txt
