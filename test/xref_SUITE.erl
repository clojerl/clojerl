-module(xref_SUITE).

-type config() :: proplists:proplist().

-export(
   [
    all/0,
    xref/1
   ]
  ).

-spec all() -> [atom()].
all() ->
  ExcludedFuns = [all, module_info],
  Exports = ?MODULE:module_info(exports),
  [F || {F, 1} <- Exports, not lists:member(F, ExcludedFuns)].

-spec xref(config()) -> {comment, []}.
xref(_Config) ->
  Dirs = [filename:absname("../../ebin")],
  [] = xref_runner:check(undefined_function_calls, #{dirs => Dirs}),
  [] = xref_runner:check(undefined_functions, #{dirs => Dirs}),
  [] = xref_runner:check(locals_not_used, #{dirs => Dirs}),
  [] = xref_runner:check(deprecated_function_calls, #{dirs => Dirs}),
  [] = xref_runner:check(deprecated_functions, #{dirs => Dirs}),
  {comment, ""}.
