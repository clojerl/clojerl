PROJECT = clojerl

DEPS = sync
dep_sync = git https://github.com/inaka/sync.git 0.1.3

TEST_DEPS = xref_runner
dep_xref_runner = git https://github.com/inaka/xref_runner.git 0.2.2

include erlang.mk

CT_OPTS = -cover test/clojerl.spec

COMPILE_FIRST = lang/protocols/*

SHELL_OPTS += -name ${PROJECT}@`hostname` -s ${PROJECT} -s sync

# Generate a list of all modules for the cover.spec
print-all-modules:
	find src -type f -name "*.erl" | xargs basename | sed -e 's/\.erl//' | sed -e "s/\(.*\)/'\1',/"
