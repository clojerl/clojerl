-module(erlang_util_UUID_SUITE).

-include("clojerl.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).

-export([str/1, hash/1]).

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

-spec hash(config()) -> result().
hash(_Config) ->
  UUIDStr1 = <<"01234567-ABCD-ABCD-ABCD-0123456789AB">>,
  UUID1    = 'erlang.util.UUID':?CONSTRUCTOR(UUIDStr1),
  Hash1    = 'clojerl.IHash':hash(UUID1),

  UUIDStr2 = <<"01234567-FFFF-ABCD-ABCD-0123456789AB">>,
  UUID2    = 'erlang.util.UUID':?CONSTRUCTOR(UUIDStr2),
  Hash2    = 'clojerl.IHash':hash(UUID2),

  true     = Hash2 =/= Hash1,

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  UUIDStr = <<"01234567-ABCD-ABCD-ABCD-0123456789AB">>,

  UUID    = 'erlang.util.UUID':?CONSTRUCTOR(UUIDStr),
  UUIDStr = clj_core:str(UUID),

  ct:comment("Invalid UUIDs"),
  ok = try
         'erlang.util.UUID':?CONSTRUCTOR(
                               <<"01234567-ABCD-ABCD-ABCD-0123456789ABZ">>
                              ),
         error
       catch _:_ ->
           ok
       end,

  ok = try
         'erlang.util.UUID':?CONSTRUCTOR(
                               <<"01234567-ABCD-ABCD-ABCD-012345678,AB">>
                              ),
         error
       catch _:_ ->
           ok
       end,

  ok = try
         'erlang.util.UUID':?CONSTRUCTOR(
                               <<"0123456-ABCD-ABCD-ABCD-0123456789AB">>
                              ),
         error
       catch _:_ ->
           ok
       end,

  {comments, ""}.
