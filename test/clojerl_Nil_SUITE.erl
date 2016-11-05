-module(clojerl_Nil_SUITE).

-include("clojerl.hrl").

-export([all/0, init_per_suite/1]).

-export([ cons/1
        , hash/1
        , seq/1
        , str/1
        , complete_coverage/1
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
  application:ensure_all_started(clojerl),
  Config.

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------

-spec cons(config()) -> result().
cons(_Config) ->
  Nil = ?NIL,

  ct:comment("Conj an element to nil"),
  OneList = clj_core:conj(Nil, 1),

  1    = clj_core:count(OneList),
  true = clj_core:equiv(OneList, [1]),

  {comments, ""}.

-spec hash(config()) -> result().
hash(_Config) ->
  HashNil = 'clojerl.IHash':hash(?NIL),
  true = is_integer(HashNil),

  {comments, ""}.

-spec seq(config()) -> result().
seq(_Config) ->
  Nil = ?NIL,

  ?NIL = clj_core:seq(Nil),

  ?NIL = clj_core:first(Nil),
  ?NIL = clj_core:next(Nil),
  [] = clj_core:rest(Nil),

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  ct:comment("Check the string representation of true and false"),
  <<"">> = clj_core:str(?NIL),

  {comments, ""}.

-spec complete_coverage(config()) -> result().
complete_coverage(_Config) ->
  [] = 'clojerl.Nil':to_list(?NIL),

  {comments, ""}.
