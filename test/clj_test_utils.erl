-module(clj_test_utils).

-export([relative_path/1]).

-spec relative_path(binary()) -> binary().
relative_path(Path) ->
  Root = list_to_binary(code:lib_dir(clojerl)),
  <<Root/binary, "/", Path/binary>>.
