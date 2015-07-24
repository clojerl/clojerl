PROJECT = clojerl

DEPS = sync zipper

dep_zipper = git git://github.com/inaka/zipper 0.1.2
dep_sync = git git://github.com/inaka/sync.git 0.1.3

include erlang.mk

CT_OPTS = -cover test/clojerl.spec


SHELL_OPTS += -name ${PROJECT}@`hostname` -s sync
