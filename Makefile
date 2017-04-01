.PHONY: test shell clojure.core clojure.main clojure bootstrap

all:
	@rebar3 compile

test:
	@rebar3 do ct, cover, cover_result

shell:
	@rebar3 as dev shell --sname clojerl-shell --setcookie clojerl

clean:
	@rebar3 clean

clean-all: clean
	@rm -rf _build/ rebar.lock ebin/

CLJ_SRC ?= src/clj
CLJ_TARGET ?= ebin
CLJ_FILES=$(filter-out ${CLJ_EXCLUDE},$(wildcard ${CLJ_SRC}/**/*.clj))

CODE_PATH=_build/default/lib/*/ebin ${CLJ_TARGET} ${CLJ_SRC}
TEST_CODE_PATH=_build/default/lib/*/ebin ${CLJ_TARGET} ${CLJ_SRC}

tests-shell: SHELL_OPTS = -sname clojerl-test-shell -setcookie clojerl -s clojerl +pc unicode
tests-shell:
	@erl -pa ${TEST_CODE_PATH} ${SHELL_OPTS}

repl: SHELL_OPTS = -sname clojerl-repl -setcookie clojerl -s clojerl
repl: SHELL_OPTS += -eval "'clojure.main':main([<<\"-r\">>])." -s clojerl start -noshell +pc unicode
repl: clojure.core clojure.main
	@rlwrap erl -pa ${CODE_PATH} ${SHELL_OPTS}

# ------------------------------------------------------------------------------
# Clojure files compilation
# ------------------------------------------------------------------------------

define clj_to_beam
$(subst .clj,.beam,$(subst _,-,$(subst ${CLJ_SRC}.,${CLJ_TARGET}/,$(subst /,.,$(1)))))
endef

define compile_clojure_template
$(call clj_to_beam,$(1)): $(1)
	@echo "$(1)"
	@bin/compile $(1)
endef

bootstrap: all
	bin/compile ${CLJ_SRC}/clojure/core.clj

clojure.core: $(call clj_to_beam,${CLJ_SRC}/clojure/core.clj)
clojure.main: $(call clj_to_beam,${CLJ_SRC}/clojure/main.clj)

clojure: all $(call clj_to_beam,${CLJ_FILES})

$(foreach clj,${CLJ_FILES},$(eval $(call compile_clojure_template,${clj})))

benchmark: bootstrap
	@bin/compile ${CLJ_SRC}/benchmark/benchmark_runner.clj | \
	tee ${CLJ_SRC}/benchmark/result.txt
