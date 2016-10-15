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

-export([?CONSTRUCTOR/1]).

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
-export(['_'/1]).
-export([ seq/1
        , to_list/1
        ]).
-export([str/1]).

-type type() :: #?TYPE{}.

-spec ?CONSTRUCTOR(function()) -> type().
?CONSTRUCTOR(Fn) when is_function(Fn) ->
  #?TYPE{data = Fn}.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

count(#?TYPE{name = ?M, data = Fn}) ->
  case Fn([]) of
    undefined -> 0;
    Seq       -> 'clojerl.Counted':count(Seq)
  end.

cons(#?TYPE{name = ?M} = LazySeq, X) ->
  'clojerl.Cons':?CONSTRUCTOR(X, LazySeq).

empty(_) -> [].

equiv( #?TYPE{name = ?M, data = X}
     , #?TYPE{name = ?M, data = Y}
     ) ->
  clj_core:equiv(X, Y);
equiv(#?TYPE{name = ?M} = LazySeq, Y) ->
  case clj_core:'sequential?'(Y) of
    true  -> clj_core:equiv('clojerl.Seqable':to_list(LazySeq), Y);
    false -> false
  end.

hash(#?TYPE{name = ?M} = LazySeq) ->
  clj_murmur3:ordered(LazySeq).

meta(#?TYPE{name = ?M, info = Info}) ->
  maps:get(meta, Info, undefined).

with_meta(#?TYPE{name = ?M, info = Info} = List, Metadata) ->
  List#?TYPE{info = Info#{meta => Metadata}}.

first(#?TYPE{name = ?M, data = Fn}) ->
  case Fn([]) of
    undefined -> undefined;
    #?TYPE{name = ?M} = LazySeq -> first(LazySeq);
    Seq -> 'clojerl.ISeq':first(Seq)
  end.

next(#?TYPE{name = ?M, data = Fn}) ->
  case Fn([]) of
    undefined -> undefined;
    #?TYPE{name = ?M} = LazySeq -> next(LazySeq);
    Seq -> 'clojerl.ISeq':next(Seq)
  end.

more(#?TYPE{name = ?M, data = Fn}) ->
  case Fn([]) of
    undefined -> [];
    #?TYPE{name = ?M} = LazySeq -> more(LazySeq);
    Seq -> 'clojerl.ISeq':more(Seq)
  end.

'_'(_) -> undefined.

seq(#?TYPE{name = ?M, data = Fn}) ->
  case Fn([]) of
    undefined ->
      undefined;
    #?TYPE{name = ?M} = LazySeq ->
      seq(LazySeq);
    Seq ->
      'clojerl.Seqable':seq(Seq)
  end.

to_list(#?TYPE{name = ?M} = LazySeq) ->
  do_to_list(LazySeq, []).

do_to_list(undefined, Acc) ->
  lists:reverse(Acc);
do_to_list(Seq0, Acc) ->
  case 'clojerl.Seqable':seq(Seq0) of
    undefined -> do_to_list(undefined, Acc);
    Seq ->
      First = 'clojerl.ISeq':first(Seq),
      Rest  = 'clojerl.ISeq':next(Seq),
      do_to_list(Rest, [First | Acc])
  end.

str(#?TYPE{name = ?M, data = Fn}) ->
  {uniq, Uniq} = erlang:fun_info(Fn, uniq),
  UniqBin = 'clojerl.Stringable':str(Uniq),
  <<"#<clojerl.LazySeq@", UniqBin/binary, ">">>.
