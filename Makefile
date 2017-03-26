include clojure.mk

CODE_PATH=_build/default/lib/*/ebin ebin priv
TEST_CODE_PATH=_build/default/lib/*/ebin ebin priv


.PHONY: test shell

all:
	@rebar3 compile

test:
	@rebar3 do ct, cover, cover_result

shell:
	@rebar3 as dev shell --sname clojerl-shell --setcookie clojerl

tests-shell: SHELL_OPTS = -sname clojerl-test-shell -setcookie clojerl -s clojerl +pc unicode
tests-shell:
	@erl -pa ${TEST_CODE_PATH} -pa test/compiler -pa deps/*/ebin ${SHELL_OPTS}

repl: SHELL_OPTS = -sname clojerl-repl -setcookie clojerl -s clojerl
repl: SHELL_OPTS += -eval "'clojure.main':main([<<\"-r\">>])." -s clojerl start -noshell +pc unicode
repl: clojure.core clojure.main
	@rlwrap erl -pa ${CODE_PATH} ${SHELL_OPTS}

shell-no-sync: SHELL_OPTS = -pa  -sname clojerl -setcookie clojerl -s clojerl +pc unicode
shell-no-sync: shell;
