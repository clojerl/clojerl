-module(erlang_util_Regex_SUITE).

-include("clojerl.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).

-export([ str/1
        , hash/1
        ]).

-type config() :: list().
-type result() :: {comments, string()}.

-spec all() -> [atom()].
all() ->
  ExcludedFuns = [init_per_suite, end_per_suite, all, module_info],
  Exports = ?MODULE:module_info(exports),
  [F || {F, 1} <- Exports, not lists:member(F, ExcludedFuns)].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  clojerl:start(),
  Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  application:stop(clojerl),
  Config.

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------

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
