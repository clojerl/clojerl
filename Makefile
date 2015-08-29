PROJECT = clojerl

DEPS = sync

dep_sync = git git://github.com/inaka/sync.git 0.1.3

include erlang.mk

CT_OPTS = -cover test/clojerl.spec

COMPILE_FIRST = clojerl.Named

SHELL_OPTS += -name ${PROJECT}@`hostname` -s sync
