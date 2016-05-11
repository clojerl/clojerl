PROJECT = clojerl

SHELL_DEPS = sync eep eper eflame
TEST_DEPS = xref_runner
COVER_SPEC = test/clojerl.spec

dep_sync        = git https://github.com/jfacorro/sync.git     jfacorro.clj_support
dep_eep         = git https://github.com/virtan/eep            v1.1
dep_eper        = git https://github.com/massemanet/eper       0.97.3
dep_eflame      = git https://github.com/proger/eflame         master
dep_xref_runner = git https://github.com/inaka/xref_runner.git 0.2.5

include erlang.mk

CT_OPTS = -cover ${COVER_SPEC} -erl_args -s ${PROJECT}

COMPILE_FIRST = lang/protocols/*

SHELL_OPTS += -pa priv -name ${PROJECT}@`hostname` -setcookie clojerl -s ${PROJECT} -s sync

# Generate a list of all modules for the cover.spec
print-all-modules:
	@find src -type f -name "*.erl" | xargs basename -s .erl | sort | sed -e "s/\(.*\)/'\1',/"

coverage-results:
	@erl -noshell -pa ebin -eval 'cover_report:report(), halt(0).' -env COVER_SPEC '${COVER_SPEC}'

coverage-html:
	@find . -name "cover.html" | sort -r | head -1 | xargs open

tests-shell: test-build
	@erl -pa ebin -pa test -pa test/compiler -pa deps/*/ebin ${SHELL_OPTS} # -s sync

repl: SHELL_OPTS += -s clj_repl repl -noshell
repl:
	@rlwrap erl -pa ebin -pa deps/*/ebin -pa priv ${SHELL_OPTS}

shell-no-sync: SHELL_OPTS = -pa priv -name ${PROJECT}@`hostname` -setcookie clojerl -s ${PROJECT}
shell-no-sync: shell;
