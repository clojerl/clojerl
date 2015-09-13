PROJECT = clojerl

COVER_SPEC = test/clojerl.spec

DEPS = sync
dep_sync = git https://github.com/inaka/sync.git 0.1.3

TEST_DEPS = xref_runner
dep_xref_runner = git https://github.com/inaka/xref_runner.git 0.2.2

include erlang.mk

CT_OPTS = -cover ${COVER_SPEC}

COMPILE_FIRST = lang/protocols/*

SHELL_OPTS += -name ${PROJECT}@`hostname` -s ${PROJECT} -s sync

# Generate a list of all modules for the cover.spec
print-all-modules:
	find src -type f -name "*.erl" | xargs basename | sed -e 's/\.erl//' | sed -e "s/\(.*\)/'\1',/"

report-cover:
	@erl -noshell -pa ebin -eval 'cover_report:report(), halt(0).' -env COVER_SPEC '${COVER_SPEC}'
