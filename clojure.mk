# Clojure files compilation

CLJ_SRC ?= priv
#CLJ_EXCLUDE=${CLJ_SRC}/clojure/core_print.clj ${CLJ_SRC}/clojure/core_deftype.clj
CLJ_FILES=$(filter-out ${CLJ_EXCLUDE},$(wildcard ${CLJ_SRC}/**/*.clj))

define clj_to_beam
$(subst .clj,.beam,$(subst _,-,$(subst ${CLJ_SRC}.,ebin/,$(subst /,.,$(1)))))
endef

define compile_clojure_template
$(call clj_to_beam,$(1)): $(1)
	@echo "$(1)"
	@bin/compile $(1)
endef

.PHONY: clojure.core clojure.main clojure bootstrap-clj

bootstrap-clj: all
	bin/compile ${CLJ_SRC}/clojure/core.clj

clojure.core: $(call clj_to_beam,priv/clojure/core.clj)
clojure.main: $(call clj_to_beam,priv/clojure/main.clj)

clojure: all $(call clj_to_beam,${CLJ_FILES})

$(foreach clj,${CLJ_FILES},$(eval $(call compile_clojure_template,${clj})))
