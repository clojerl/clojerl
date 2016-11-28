-module(erlang_util_Regex_SUITE).

-include("clojerl.hrl").
-include("clj_test_utils.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).

-export([ replace/1
        , quote/1
        , split/1
        ]).

-export([ str/1
        , hash/1
        ]).

-spec all() -> [atom()].
all() -> clj_test_utils:all(?MODULE).

-spec init_per_suite(config()) -> config().
init_per_suite(Config) -> clj_test_utils:init_per_suite(Config).

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  Config.

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------

-spec replace(config()) -> result().
replace(_Config) ->
  Regex = 'erlang.util.Regex':?CONSTRUCTOR(<<"a">>),

  <<"foa">> = 'erlang.util.Regex':replace(Regex, <<"faa">>, <<"o">>, []),
  <<"foo">> = 'erlang.util.Regex':replace(Regex, <<"faa">>, <<"o">>, [global]),
  <<"foo">> = 'erlang.util.Regex':replace(Regex, <<"foo">>, <<"e">>, []),
  <<>> = 'erlang.util.Regex':replace(Regex, <<>>, <<"e">>, []),

  ReBin = <<"a">>,
  <<"foa">> = 'erlang.util.Regex':replace(ReBin, <<"faa">>, <<"o">>, []),
  <<"foo">> = 'erlang.util.Regex':replace(ReBin, <<"faa">>, <<"o">>, [global]),
  <<"foo">> = 'erlang.util.Regex':replace(ReBin, <<"foo">>, <<"e">>, []),
  <<>> = 'erlang.util.Regex':replace(Regex, <<>>, <<"e">>, []),

  {comments, ""}.

-spec quote(config()) -> result().
quote(_Config) ->
  <<"f\\$\\$">>   = 'erlang.util.Regex':quote(<<"f$$">>),
  <<"f\\\\\\\\">> = 'erlang.util.Regex':quote(<<"f\\\\">>),

  {comments, ""}.

-spec split(config()) -> result().
split(_Config) ->
  Regex = 'erlang.util.Regex':?CONSTRUCTOR(<<"-">>),

  [<<"1">>, <<"2">>] = 'erlang.util.Regex':split(Regex, <<"1-2">>, []),
  [<<"1">>, <<"2">>] = 'erlang.util.Regex':split(<<"-">>, <<"1-2">>, []),

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  Regex = 'erlang.util.Regex':?CONSTRUCTOR(<<"abcd">>),
  <<"#\"abcd\"">> = clj_core:str(Regex),

  Result = {match, [<<"abcd">>]},
  Result = 'erlang.util.Regex':run( Regex
                                  , <<"abcdabcd">>
                                  , [{capture, all, binary}]
                                  ),

  {comments, ""}.

-spec hash(config()) -> result().
hash(_Config) ->
  Regex1 = 'erlang.util.Regex':?CONSTRUCTOR(<<"abcd">>),
  Regex2 = 'erlang.util.Regex':?CONSTRUCTOR(<<"abcd">>),
  Regex3 = 'erlang.util.Regex':?CONSTRUCTOR(<<"abcd.">>),

  true  = 'clojerl.IHash':hash(Regex1) == 'clojerl.IHash':hash(Regex2),
  false = 'clojerl.IHash':hash(Regex3) == 'clojerl.IHash':hash(Regex2),

  {comments, ""}.
