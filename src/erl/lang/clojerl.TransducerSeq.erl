-module('clojerl.TransducerSeq').

-include("clojerl.hrl").

-behavior('clojerl.IEquiv').
-behavior('clojerl.ISeq').
-behavior('clojerl.ISeqable').
-behavior('clojerl.ISequential').
-behavior('clojerl.IStringable').

-clojure(true).

-export([?CONSTRUCTOR/2]).

-export([equiv/2]).
-export([ first/1
        , next/1
        , more/1
        ]).
-export([ seq/1
        , to_list/1
        ]).
-export(['_'/1]).
-export([str/1]).

-type type() :: #{ ?TYPE => ?M
                 , xform => any()
                 , coll  => any()
                 , next  => none | {next, any()}
                 }.

-spec ?CONSTRUCTOR(any(), any()) -> type().
?CONSTRUCTOR(XForm, Coll) ->
  UUID = 'erlang.util.UUID':random(),
  Key  = 'erlang.util.UUID':str(UUID),
  F = fun
        ([]) -> ?NIL;
        ([Acc]) -> Acc;
        ([Acc, Item]) ->
          erlang:put(Key, [Item | erlang:get(Key)]),
          Acc
      end,
  XForm1 = clj_rt:apply(XForm, [F]),
  X = #{ ?TYPE     => ?M
       , xform     => XForm1
       , coll      => clj_rt:seq(Coll)
       , key       => Key
       , buffer    => []
       , completed => false
       },
  step(X).

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

%% clojerl.IEquiv

equiv(#{?TYPE := ?M} = X, Y) ->
  clj_rt:equiv(to_list(X), Y).

%% clojerl.ISeq

first(#{?TYPE := ?M, buffer := []}) ->
  ?NIL;
first(#{?TYPE := ?M, buffer := [Current | _]}) ->
  Current.

next(#{?TYPE := ?M, buffer := Buffer0} = X0) ->
  Buffer1 = case Buffer0 of
              [] -> Buffer0;
              [_ | BufferRest] -> BufferRest
            end,
  case step(X0#{buffer => Buffer1}) of
    #{completed := true, buffer := []} -> ?NIL;
    X1 -> X1
  end.

more(#{?TYPE := ?M, completed := true, buffer := []}) ->
  ?NIL;
more(#{?TYPE := ?M, buffer := []} = X) ->
  step(X);
more(#{?TYPE := ?M, buffer := [_ | Buffer]} = X) ->
  step(X#{buffer => Buffer}).

%% clojerl.ISeqable

seq(#{?TYPE := ?M, completed := true, buffer := []}) ->
  ?NIL;
seq(#{?TYPE := ?M} = X) ->
  step(X).

to_list(#{?TYPE := ?M} = X) ->
  do_to_list(seq(X), []).

%% clojerl.ISequential

'_'(#{?TYPE := ?M}) -> ok.

%% clojerl.IStringable

str(#{?TYPE := ?M} = X) ->
  List = to_list(X),
  clj_rt:print_str(clj_rt:list(List)).

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

step(#{completed := true} = X) ->
  X;
step(#{xform := XForm, coll := ?NIL} = X) ->
  clj_rt:apply(XForm, [?NIL]),
  X#{coll := ?NIL, completed := true};
step(#{ buffer := []
      , xform  := XForm
      , coll   := Coll0
      , key    := Key
      } = X0) ->
  First = clj_rt:first(Coll0),
  Coll1 = clj_rt:next(Coll0),
  {Result, Value} = fetch_item(XForm, First, Key),
  Completed = case 'clojerl.Reduced':is_reduced(Result) of
                true  -> clj_rt:apply(XForm, [?NIL]), true;
                false -> false
              end,
  X1 = X0#{coll := Coll1, completed := Completed},
  case Value of
    [] -> step(X1);
    _  -> X1#{buffer := lists:reverse(Value)}
  end;
step(X) ->
  X.

fetch_item(XForm, X, Key) ->
  try
    erlang:put(Key, []),
    Result = clj_rt:apply(XForm, [?NIL, X]),
    {Result, erlang:get(Key)}
  after
    erlang:erase(Key)
  end.

do_to_list(?NIL, List) ->
  lists:reverse(List);
do_to_list(X, List) ->
  do_to_list(next(X), [first(X) | List]).
