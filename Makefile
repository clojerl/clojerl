PROJECT = clojerl

SHELL_DEPS = sync
TEST_DEPS = xref_runner
COVER_SPEC = test/clojerl.spec

dep_sync = git https://github.com/rustyio/sync.git 9c78e7b
dep_xref_runner = git https://github.com/inaka/xref_runner.git 0.2.5

include erlang.mk

CT_OPTS = -cover ${COVER_SPEC} -erl_args -s ${PROJECT}

COMPILE_FIRST = lang/protocols/*

SHELL_OPTS += -name ${PROJECT}@`hostname` -setcookie clojerl -s ${PROJECT} -s sync

# Generate a list of all modules for the cover.spec
print-all-modules:
	find src -type f -name "*.erl" | xargs basename | sort | sed -e 's/\.erl//' | sed -e "s/\(.*\)/'\1',/"

report-cover:
	@erl -noshell -pa ebin -eval 'cover_report:report(), halt(0).' -env COVER_SPEC '${COVER_SPEC}'

coverage-results:
	@find . -name "cover.html" | sort -r | head -1 | xargs open

tests-shell: build-ct-suites
	@erl -pa ebin -pa test -pa test/compiler -pa deps/*/ebin ${SHELL_OPTS} -s sync
