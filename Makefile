.PHONY: test shell clojure.core clojure.main clojure bootstrap

all:
	@rebar3 compile

test:
	@rebar3 do ct, cover, cover_result

shell:
	@rebar3 as dev shell --sname clojerl-shell --setcookie clojerl

CODE_PATH=_build/default/lib/*/ebin ebin priv
TEST_CODE_PATH=_build/default/lib/*/ebin ebin priv

tests-shell: SHELL_OPTS = -sname clojerl-test-shell -setcookie clojerl -s clojerl +pc unicode
tests-shell:
	@erl -pa ${TEST_CODE_PATH} -pa test/compiler -pa deps/*/ebin ${SHELL_OPTS}

repl: SHELL_OPTS = -sname clojerl-repl -setcookie clojerl -s clojerl
repl: SHELL_OPTS += -eval "'clojure.main':main([<<\"-r\">>])." -s clojerl start -noshell +pc unicode
repl: clojure.core clojure.main
	@rlwrap erl -pa ${CODE_PATH} ${SHELL_OPTS}

shell-no-sync: SHELL_OPTS = -pa  -sname clojerl -setcookie clojerl -s clojerl +pc unicode
shell-no-sync: shell;

# Clojure files compilation

CLJ_SRC ?= priv
CLJ_FILES=$(filter-out ${CLJ_EXCLUDE},$(wildcard ${CLJ_SRC}/**/*.clj))

define clj_to_beam
$(subst .clj,.beam,$(subst _,-,$(subst ${CLJ_SRC}.,ebin/,$(subst /,.,$(1)))))
endef

define compile_clojure_template
$(call clj_to_beam,$(1)): $(1)
	@echo "$(1)"
	@bin/compile $(1)
endef

bootstrap: all
	bin/compile ${CLJ_SRC}/clojure/core.clj

clojure.core: $(call clj_to_beam,priv/clojure/core.clj)
clojure.main: $(call clj_to_beam,priv/clojure/main.clj)

clojure: all $(call clj_to_beam,${CLJ_FILES})

$(foreach clj,${CLJ_FILES},$(eval $(call compile_clojure_template,${clj})))

benchmark: bootstrap
	@bin/compile ${CLJ_SRC}/benchmark/benchmark_runner.clj | \
	tee ${CLJ_SRC}/benchmark/result.txt
