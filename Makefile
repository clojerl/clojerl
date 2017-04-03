REBAR3      := rebar3
ROOT_SRC    := src
CLJ_SRC     := ${ROOT_SRC}/clj
CLJ_TEST    := ${ROOT_SRC}/../test/clj
CLJ_TARGET  := _build/default/lib/clojerl/ebin
CLJ_EXCLUDE := ${CLJ_SRC}/clojure/core_deftype.clj ${CLJ_SRC}/clojure/core_print.clj
CLJ_FILES   := $(filter-out ${CLJ_EXCLUDE},$(wildcard ${CLJ_SRC}/**/*.clj))

CODE_PATH   := ${CLJ_TARGET} ${CLJ_SRC}
COMPILE     := bin/compile -o ${CLJ_TARGET} -pa ${CLJ_SRC} -pa ${CLJ_TEST}

.PHONY: default erl test shell clean

default: compile

compile:
	@${REBAR3} compile

test:
	@${REBAR3} as test do ct, cover, cover_result

shell:
	@${REBAR3} as dev shell --sname clojerl-shell --setcookie clojerl

clean:
	@${REBAR3} clean
	@rm -rf _build/ rebar.lock ebin/

repl: SHELL_OPTS = -sname clojerl-repl -setcookie clojerl -s clojerl
repl: SHELL_OPTS += -eval "'clojure.main':main([<<\"-r\">>])." -s clojerl start -noshell +pc unicode
repl: clojure.core clojure.main
	@rlwrap erl -pa ${CODE_PATH} ${SHELL_OPTS}

# ------------------------------------------------------------------------------
# Clojure files compilation
# ------------------------------------------------------------------------------

# Maps clj paths to beam paths
# path/to/ns/file.clj -> path/to/ebin/ns.file.beam
define clj_to_beam
$(addprefix ${CLJ_TARGET}/,$(subst .clj,.beam,$(subst _,-,$(subst /,.,$(1:${CLJ_SRC}/%=%)))))
endef

# Creates target for the beam file
define compile_clojure_template
$(call clj_to_beam,${1}): ${1}
	@echo Compiling $(1:${CURDIR}/%=%)
	@${COMPILE} ${1}
endef

# Maps clj paths to ns names
# path/to/ns/file.clj -> ns.file
define clj_to_ns
$(subst .clj,,$(subst /,.,$(1:${CLJ_SRC}/%=%)))
endef

# Creates target with the ns name
define compile_clojure_template_ns
$(call clj_to_ns,${1}): $(call clj_to_beam,${1})
endef

# Targets for beam files
$(foreach clj,${CLJ_FILES},$(eval $(call compile_clojure_template,${clj})))

# Phony targets with namespaces
CLJ_NAMESPACES = $(foreach clj,${CLJ_FILES},$(call clj_to_ns,${clj}))
.PHONY: ${CLJ_NAMESPACES}
$(foreach clj,${CLJ_FILES},$(eval $(call compile_clojure_template_ns,${clj})))

# Add specific target for clojure.core that doesn't have the cloure.core.beam
clojure.core:
	@echo Compiling src/clj/clojure/core.clj
	@${COMPILE} src/clj/clojure/core.clj

$(filter-out clojure.core,${CLJ_NAMESPACES}): clojure.core

.PHONY: bootstrap clojure

CLJ_FILES_FILTER = $(filter-out %/clojure/core.clj,${CLJ_FILES})

bootstrap: erl clojure.core

clojure: bootstrap $(call clj_to_beam,${CLJ_FILES_FILTER})

benchmark: bootstrap
	@${COMPILE} ${CLJ_TEST}/benchmark/benchmark_runner.clj | \
	tee ${CLJ_TEST}/benchmark/result.txt
