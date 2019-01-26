-module('clojerl.TransducerSeq').

-include("clojerl.hrl").

-behavior('clojerl.IEquiv').
-behavior('clojerl.ISeq').
-behavior('clojerl.ISeqable').
-behavior('clojerl.ISequential').
-behavior('clojerl.IStringable').

-clojure(true).

-export([?CONSTRUCTOR/2, ?CONSTRUCTOR/3]).

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

-type type() :: #{ ?TYPE     => ?M
                 , xform     => any()
                 , coll      => any()
                 , key       => binary()
                 , buffer    => [any()]
                 , completed => boolean()
                 , multi     => boolean()
                 }.

-spec ?CONSTRUCTOR(any(), any()) -> type().
?CONSTRUCTOR(XForm, Coll) ->
  ?CONSTRUCTOR(XForm, Coll, false).

-spec ?CONSTRUCTOR(any(), any(), boolean()) -> type().
?CONSTRUCTOR(XForm, Coll, Multi) ->
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
       , multi     => Multi
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
step(#{ xform  := XForm
      , coll   := ?NIL
      , key    := Key
      , buffer := Buffer
      } = X) ->
  {_, Value} = fetch_item(XForm, [?NIL], Key),
  X#{ completed := true
    , buffer    := Buffer ++ lists:reverse(Value)
    };
step(#{ xform  := XForm
      , coll   := Coll0
      , key    := Key
      , buffer := []
      , multi  := Multi
      } = X0) ->
  First = clj_rt:first(Coll0),
  Coll1 = clj_rt:next(Coll0),
  Args  = case Multi of
            true -> [?NIL | clj_rt:to_list(First)];
            false -> [?NIL, First]
          end,
  {Result, Value0} = fetch_item(XForm, Args, Key),
  Completed = 'clojerl.Reduced':is_reduced(Result),
  Value1    = case Completed of
                true  ->
                  {_, Value_} = fetch_item(XForm, [?NIL], Key),
                  Value_ ++ Value0;
                false ->
                  Value0
              end,
  X1 = X0#{coll := Coll1, completed := Completed},
  case Value1 of
    [] -> step(X1);
    _  -> X1#{buffer := lists:reverse(Value1)}
  end;
step(X) ->
  X.

fetch_item(XForm, Args, Key) ->
  try
    erlang:put(Key, []),
    Result = clj_rt:apply(XForm, Args),
    {Result, erlang:get(Key)}
  after
    erlang:erase(Key)
  end.

do_to_list(?NIL, List) ->
  lists:reverse(List);
do_to_list(X, List) ->
  do_to_list(next(X), [first(X) | List]).
