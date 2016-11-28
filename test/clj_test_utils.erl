-module(clj_test_utils).

-include("clj_test_utils.hrl").

-export([ all/1
        , all/2
        , init_per_suite/1
        , relative_path/1
        ]).

-spec all(module()) -> [atom()].
all(Module) -> all(Module, []).

-spec all(module(), [atom()]) -> [atom()].
all(Module, Functions) ->
  ExcludedFuns = [init_per_suite, end_per_suite, all, module_info | Functions],
  Exports = Module:module_info(exports),
  [F || {F, 1} <- Exports, not lists:member(F, ExcludedFuns)].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  clojerl:start(),
  Config.

-spec relative_path(binary()) -> binary().
relative_path(Path) ->
  Root = list_to_binary(code:lib_dir(clojerl)),
  <<Root/binary, "/", Path/binary>>.
