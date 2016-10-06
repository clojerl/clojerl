-module(clj_test_utils).

-export([relative_path/1]).

-spec relative_path(binary()) -> binary().
relative_path(Path) -> <<"../../", Path/binary>>.
