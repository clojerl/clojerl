-module(clojerl_Atom_SUITE).

-include("clojerl.hrl").
-include("clj_test_utils.hrl").

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        ]).

-export([ deref/1
        , swap/1
        , reset/1
        , compare_and_set/1
        , equiv/1
        , meta/1
        , str/1
        , complete_coverage/1
        ]).

-spec all() -> [atom()].
all() -> clj_test_utils:all(?MODULE).

-spec init_per_suite(config()) -> config().
init_per_suite(Config) -> clj_test_utils:init_per_suite(Config).

-spec end_per_suite(config()) -> config().
end_per_suite(Config) -> Config.

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------

-spec deref(config()) -> result().
deref(_Config) ->
  Atom = 'clojerl.Atom':?CONSTRUCTOR(1),

  ct:comment("deref an atom"),
  1 = clj_rt:deref(Atom),

  2 = 'clojerl.Atom':reset(Atom, 2),
  2 = clj_rt:deref(Atom),

  {comments, ""}.

-spec swap(config()) -> result().
swap(_Config) ->
  Atom = 'clojerl.Atom':?CONSTRUCTOR(2),

  ct:comment("Successful swaps"),
  3 = 'clojerl.Atom':swap(Atom, fun(X) -> X + 1 end),
  4 = 'clojerl.Atom':swap(Atom, fun(X, Y) -> X + Y end, 1),
  6 = 'clojerl.Atom':swap(Atom, fun(X, Y, Z) -> X + Y + Z end, 1, 1),
  9 = 'clojerl.Atom':swap(Atom, fun(X, Y, Z, W) -> X + Y + Z + W end, 1, 1, [1]),

  ct:comment("Concurrent swaps"),
  Inc      = fun(X) -> X + 1 end,
  Self     = self(),
  ResetFun = fun(N) ->
                 spawn(fun() -> 'clojerl.Atom':swap(Atom, Inc), Self ! ok end)
             end,
  N        = 100,
  Result   = N + 9,
  lists:foreach(ResetFun, lists:seq(1, N)),
  ok       = wait_for(ok, N, 1000),
  Result   = 'clojerl.Atom':deref(Atom),

  {comments, ""}.

-spec reset(config()) -> result().
reset(_Config) ->
  Atom = 'clojerl.Atom':?CONSTRUCTOR(1),

  ct:comment("Successful resets"),
  2   = 'clojerl.Atom':reset(Atom, 2),
  foo = 'clojerl.Atom':reset(Atom, foo),
  bar = 'clojerl.Atom':reset(Atom, bar),
  <<"baz">> = 'clojerl.Atom':reset(Atom, <<"baz">>),

  ct:comment("Concurrent resets"),
  Self     = self(),
  ResetFun = fun(N) ->
                 spawn(fun() -> 'clojerl.Atom':reset(Atom, N), Self ! ok end)
             end,
  N        = 100,
  lists:foreach(ResetFun, lists:seq(1, N)),
  ok = wait_for(ok, N, 1000),

  {comments, ""}.


-spec compare_and_set(config()) -> result().
compare_and_set(_Config) ->
  Atom = 'clojerl.Atom':?CONSTRUCTOR(2),

  true  = 'clojerl.Atom':compare_and_set(Atom, 2, 3),
  false = 'clojerl.Atom':compare_and_set(Atom, whatever, 3),

  {comments, ""}.

-spec equiv(config()) -> result().
equiv(_Config) ->
  Atom1 = 'clojerl.Atom':?CONSTRUCTOR(1),
  Atom2 = 'clojerl.Atom':?CONSTRUCTOR(2),

  ct:comment("Check that the same atom with different meta is equivalent"),
  Atom3 = clj_rt:with_meta(Atom1, #{a => 1}),
  Atom4 = clj_rt:with_meta(Atom1, #{b => 2}),
  true  = clj_rt:equiv(Atom3, Atom4),

  ct:comment("Check that different atoms are not equivalent"),
  false = clj_rt:equiv(Atom1, Atom2),

  ct:comment("An atom and something else"),
  false = clj_rt:equiv(Atom1, whatever),
  false = clj_rt:equiv(Atom1, #{}),
  false = clj_rt:equiv(Atom1, 1),

  {comments, ""}.

-spec meta(config()) -> result().
meta(_Config) ->
  Atom0 = 'clojerl.Atom':?CONSTRUCTOR(1),

  Atom1 = clj_rt:with_meta(Atom0, #{a => 1}),
  #{a := 1} = clj_rt:meta(Atom1),

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  Atom0 = 'clojerl.Atom':?CONSTRUCTOR(1),
  Atom1 = clj_rt:with_meta(Atom0, #{a => 1}),

  <<"#<clojerl.Atom ", _/binary>> = clj_rt:str(Atom1),

  {comments, ""}.

-spec complete_coverage(config()) -> result().
complete_coverage(_Config) ->
  Atom = 'clojerl.Atom':?CONSTRUCTOR(1),

  Hash = 'clojerl.IHash':hash(Atom),
  Hash = 'clojerl.IHash':hash(Atom),
  true = erlang:is_integer(Hash),

  {noreply, state} = 'clojerl.Atom':handle_cast(msg, state),
  {noreply, state} = 'clojerl.Atom':handle_info(msg, state),
  {ok, state}      = 'clojerl.Atom':terminate(msg, state),
  {ok, state}      = 'clojerl.Atom':code_change(msg, from, state),

  {comments, ""}.

%%------------------------------------------------------------------------------
%% Helper
%%------------------------------------------------------------------------------

-spec wait_for(any(), integer(), timeout()) -> ok | timeout.
wait_for(_Msg, 0, _Timeout) ->
  ok;
wait_for(Msg, N, Timeout) ->
  receive
    Msg -> wait_for(Msg, N - 1, Timeout)
  after Timeout ->
      timeout
  end.
