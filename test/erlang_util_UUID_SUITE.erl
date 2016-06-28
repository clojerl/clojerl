-module(erlang_util_UUID_SUITE).

-include("clojerl.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).

-export([str/1]).

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
  UUIDStr = <<"01234567-ABCD-ABCD-ABCD-0123456789AB">>,

  UUID    = 'erlang.util.UUID':new(UUIDStr),
  UUIDStr = clj_core:str(UUID),

  ct:comment("Invalid UUIDs"),
  ok = try
         'erlang.util.UUID':new(<<"01234567-ABCD-ABCD-ABCD-0123456789ABZ">>),
         error
       catch _:_ ->
           ok
       end,

  ok = try
         'erlang.util.UUID':new(<<"01234567-ABCD-ABCD-ABCD-012345678,AB">>),
         error
       catch _:_ ->
           ok
       end,

  ok = try
         'erlang.util.UUID':new(<<"0123456-ABCD-ABCD-ABCD-0123456789AB">>),
         error
       catch _:_ ->
           ok
       end,

  {comments, ""}.
