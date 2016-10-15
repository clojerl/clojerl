-module('clojerl.Vector').

-include("clojerl.hrl").

-behavior('clojerl.Associative').
-behavior('clojerl.Counted').
-behavior('clojerl.IColl').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IFn').
-behavior('clojerl.IHash').
-behavior('clojerl.ILookup').
-behavior('clojerl.IMeta').
-behavior('clojerl.Indexed').
-behavior('clojerl.ISequential').
-behavior('clojerl.IStack').
-behavior('clojerl.Seqable').
-behavior('clojerl.Stringable').

-export([?CONSTRUCTOR/1, subvec/3]).
-export([ contains_key/2
        , entry_at/2
        , assoc/3
        ]).
-export([count/1]).
-export([ cons/2
        , empty/1
        ]).
-export([equiv/2]).
-export([invoke/2]).
-export([hash/1]).
-export([ get/2
        , get/3
        ]).
-export([ meta/1
        , with_meta/2
        ]).
-export(['_'/1]).
-export([ nth/2
        , nth/3
        ]).
-export([ peek/1
        , pop/1
        ]).
-export([ seq/1
        , to_list/1
        ]).
-export([str/1]).

-type type() :: #?TYPE{}.

-spec ?CONSTRUCTOR(list()) -> type().
?CONSTRUCTOR(Items) when is_list(Items) ->
  #?TYPE{data = array:from_list(Items, undefined)}.

-spec subvec(type(), integer(), integer()) -> type().
subvec(Vector, Start, End) ->
  AddItemAtFun =
    fun(Index, Subvec) ->
        cons(Subvec, nth(Vector, Index))
    end,
  lists:foldl(AddItemAtFun, ?CONSTRUCTOR([]), lists:seq(Start, End - 1)).

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

%% clojerl.Associative

contains_key(#?TYPE{name = ?M, data = Array}, Index) ->
  Index < array:size(Array).

entry_at(#?TYPE{name = ?M, data = Array}, Index) ->
  array:get(Index, Array).

assoc(#?TYPE{ name = ?M, data = Array} = Vector, Index, Value) ->
  case  Index =< array:size(Array) of
    true  -> Vector#?TYPE{data = array:set(Index, Value, Array)};
    false -> error(<<"Index out of bounds">>)
  end.

count(#?TYPE{name = ?M, data = Array}) -> array:size(Array).

cons(#?TYPE{name = ?M, data = Array} = Vector, X) ->
  NewArray = array:set(array:size(Array), X, Array),
  Vector#?TYPE{data = NewArray}.

empty(_) -> ?CONSTRUCTOR([]).

equiv( #?TYPE{name = ?M, data = X}
     , #?TYPE{name = ?M, data = Y}
     ) ->
  case array:size(X) == array:size(Y) of
    true ->
      X1 = array:to_list(X),
      Y1 = array:to_list(Y),
      clj_core:equiv(X1, Y1);
    false -> false
  end;
equiv(#?TYPE{name = ?M, data = X}, Y) ->
  case clj_core:'sequential?'(Y) of
    true  -> clj_core:equiv(array:to_list(X), Y);
    false -> false
  end.

invoke(#?TYPE{name = ?M, data = Array}, [Index]) ->
  array:get(Index, Array);
invoke(#?TYPE{name = ?M}, Args) ->
  CountBin = integer_to_binary(length(Args)),
  throw(<<"Wrong number of args for vector, got: ", CountBin/binary>>).

hash(#?TYPE{name = ?M, data = Array}) ->
  clj_murmur3:ordered(array:to_list(Array)).

%% clojerl.ILookup

get(#?TYPE{name = ?M} = Vector, Index) ->
  get(Vector, Index, undefined).

get(#?TYPE{name = ?M, data = Array}, Index, NotFound) ->
  case Index < array:size(Array) of
    true  -> array:get(Index, Array);
    false -> NotFound
  end.

%% clojerl.IMeta

meta(#?TYPE{name = ?M, info = Info}) ->
  maps:get(meta, Info, undefined).

with_meta(#?TYPE{name = ?M, info = Info} = Vector, Metadata) ->
  Vector#?TYPE{info = Info#{meta => Metadata}}.

'_'(_) -> undefined.

nth(#?TYPE{name = ?M, data = Array}, N) ->
  array:get(N, Array).

nth(#?TYPE{name = ?M, data = Array}, N, NotFound) ->
  case N > array:size(Array) of
    true  -> NotFound;
    false -> array:get(N, Array)
  end.

peek(#?TYPE{name = ?M, data = Array}) ->
  case array:size(Array) of
    0    -> undefined;
    Size -> array:get(Size - 1, Array)
  end.

pop(#?TYPE{name = ?M, data = Array} = Vector) ->
  case array:size(Array) of
    0    -> error(<<"Can't pop empty vector">>);
    Size ->
      NewArray = array:resize(Size - 1, Array),
      Vector#?TYPE{data = NewArray}
  end.

seq(#?TYPE{name = ?M, data = Array}) ->
  case array:size(Array) of
    0 -> undefined;
    _ -> array:to_list(Array)
  end.

to_list(#?TYPE{name = ?M, data = Array}) ->
  array:to_list(Array).

str(#?TYPE{name = ?M, data = Array}) ->
  Items = lists:map(fun clj_core:str/1, array:to_list(Array)),
  Strs  = 'clojerl.String':join(Items, <<" ">>),
  <<"[", Strs/binary, "]">>.
