PROJECT = clojerl

SHELL_DEPS = sync eep eper eflame
TEST_DEPS = xref_runner meck
COVER_SPEC = test/clojerl.spec

dep_sync        = git https://github.com/jfacorro/sync.git     jfacorro.clj_support
dep_eep         = git https://github.com/virtan/eep            v1.1
dep_eper        = git https://github.com/massemanet/eper       0.97.3
dep_eflame      = git https://github.com/jfacorro/eflame       jfacorro.limit.tracing.time
dep_xref_runner = git https://github.com/inaka/xref_runner.git 0.2.5
dep_meck        = git https://github.com/eproxus/meck          0.8.4

include erlang.mk
include clojure.mk

CT_OPTS = -cover ${COVER_SPEC} -erl_args -s ${PROJECT}

COMPILE_FIRST = erlang/erlang.io.I* lang/protocols/*
DIALYZER_OPTS += -I include
SHELL_OPTS += -pa priv -name ${PROJECT}@`hostname` -setcookie clojerl -s ${PROJECT} -s sync

# Generate a list of all modules for the cover.spec
generate-spec:
	@bin/generate-spec test/clojerl.spec.in test/clojerl.spec

coverage-results:
	@erl -noshell -pa ebin -eval 'cover_report:report(), halt(0).' -env COVER_SPEC '${COVER_SPEC}'

tests-shell: test-build
	@erl -pa ebin -pa test -pa test/compiler -pa deps/*/ebin ${SHELL_OPTS}

repl: SHELL_OPTS = -pa priv -name ${PROJECT}-repl@`hostname` -setcookie clojerl -s ${PROJECT}
repl: SHELL_OPTS += -eval "'clojure.main':main([<<\"-r\">>])." -s clojerl start -noshell
repl: clojure.core clojure.main
	@rlwrap erl -pa ebin -pa deps/*/ebin -pa priv ${SHELL_OPTS}

shell-no-sync: SHELL_OPTS = -pa priv -name ${PROJECT}@`hostname` -setcookie clojerl -s ${PROJECT}
shell-no-sync: shell;
