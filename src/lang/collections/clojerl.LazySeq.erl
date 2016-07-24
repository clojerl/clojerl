-module('clojerl.LazySeq').

-include("clojerl.hrl").

-behavior('clojerl.Counted').
-behavior('clojerl.IColl').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IHash').
-behavior('clojerl.IMeta').
-behavior('clojerl.ISeq').
-behavior('clojerl.ISequential').
-behavior('clojerl.Seqable').
-behavior('clojerl.Stringable').

-export([new/1]).

-export([count/1]).
-export([ cons/2
        , empty/1
        ]).
-export([equiv/2]).
-export([hash/1]).
-export([ meta/1
        , with_meta/2
        ]).
-export([ first/1
        , next/1
        , more/1
        ]).
-export([noop/1]).
-export([seq/1]).
-export([str/1]).

-type type() :: #?TYPE{}.

-spec new(function()) -> type().
new(Fn) when is_function(Fn) ->
  #?TYPE{data = Fn}.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

count(#?TYPE{name = ?M, data = Fn}) ->
  case clj_core:invoke(Fn, []) of
    undefined -> 0;
    Seq       -> clj_core:count(Seq)
  end.

cons(#?TYPE{name = ?M} = LazySeq, X) ->
  'clojerl.Cons':new(X, LazySeq).

empty(_) -> [].

equiv( #?TYPE{name = ?M, data = X}
                      , #?TYPE{name = ?M, data = Y}
                      ) ->
  clj_core:equiv(X, Y);
equiv(#?TYPE{name = ?M} = LazySeq, Y) ->
  case clj_core:'sequential?'(Y) of
    true  -> clj_core:equiv(clj_core:seq_to_list(LazySeq), Y);
    false -> false
  end.

hash(#?TYPE{name = ?M} = LazySeq) ->
  clj_murmur3:ordered(LazySeq).

meta(#?TYPE{name = ?M, info = Info}) ->
  maps:get(meta, Info, undefined).

with_meta(#?TYPE{name = ?M, info = Info} = List, Metadata) ->
  List#?TYPE{info = Info#{meta => Metadata}}.

first(#?TYPE{name = ?M, data = Fn}) ->
  case clj_core:invoke(Fn, []) of
    undefined -> undefined;
    #?TYPE{name = ?M} = LazySeq -> first(LazySeq);
    Seq -> clj_core:first(Seq)
  end.

next(#?TYPE{name = ?M, data = Fn}) ->
  case clj_core:invoke(Fn, []) of
    undefined -> undefined;
    #?TYPE{name = ?M} = LazySeq -> next(LazySeq);
    Seq -> clj_core:next(Seq)
  end.

more(#?TYPE{name = ?M, data = Fn}) ->
  case clj_core:invoke(Fn, []) of
    undefined -> [];
    #?TYPE{name = ?M} = LazySeq -> more(LazySeq);
    Seq -> clj_core:rest(Seq)
  end.

noop(_) -> ok.

seq(#?TYPE{name = ?M, data = Fn}) ->
  case clj_core:invoke(Fn, []) of
    undefined ->
      undefined;
    #?TYPE{name = ?M} = LazySeq ->
      seq(LazySeq);
    Seq ->
      clj_core:seq(Seq)
  end.

str(#?TYPE{name = ?M, data = Fn}) ->
  {uniq, Uniq} = erlang:fun_info(Fn, uniq),
  UniqBin = clj_core:str(Uniq),
  <<"#<clojerl.LazySeq@", UniqBin/binary, ">">>.
