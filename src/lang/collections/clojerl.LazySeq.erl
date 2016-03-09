-module('clojerl.LazySeq').

-include("clojerl.hrl").

-behavior('clojerl.Counted').
-behavior('clojerl.IColl').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IMeta').
-behavior('clojerl.ISeq').
-behavior('clojerl.ISequential').
-behavior('clojerl.Seqable').
-behavior('clojerl.Stringable').

-export([new/1]).

-export(['clojerl.Counted.count'/1]).
-export([ 'clojerl.IColl.cons'/2
        , 'clojerl.IColl.empty'/1
        ]).
-export(['clojerl.IEquiv.equiv'/2]).
-export([ 'clojerl.IMeta.meta'/1
        , 'clojerl.IMeta.with_meta'/2
        ]).
-export([ 'clojerl.ISeq.first'/1
        , 'clojerl.ISeq.next'/1
        , 'clojerl.ISeq.more'/1
        ]).
-export(['clojerl.ISequential.noop'/1]).
-export(['clojerl.Seqable.seq'/1]).
-export(['clojerl.Stringable.str'/1]).

-type type() :: #?TYPE{}.

-spec new(function()) -> type().
new(Fn) when is_function(Fn) ->
  #?TYPE{data = Fn}.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

'clojerl.Counted.count'(#?TYPE{name = ?M, data = Fn}) ->
  do_count(Fn, 0).

do_count(Fn, Count) ->
  erlang:display(do_count),
  case clj_core:invoke(Fn, []) of
    undefined -> Count;
    {_, Fn1} -> do_count(Fn1, Count + 1)
  end.

'clojerl.IColl.cons'(#?TYPE{name = ?M} = LazySeq, X) ->
  'clojerl.Cons':new(X, LazySeq).

'clojerl.IColl.empty'(_) -> [].

'clojerl.IEquiv.equiv'( #?TYPE{name = ?M, data = X}
                      , #?TYPE{name = ?M, data = Y}
                      ) ->
  clj_core:equiv(X, Y);
'clojerl.IEquiv.equiv'(#?TYPE{name = ?M, data = X}, Y) ->
  case clj_core:'sequential?'(Y) of
    true  -> clj_core:equiv(X, clj_core:seq(Y));
    false -> false
  end.

'clojerl.IMeta.meta'(#?TYPE{name = ?M, info = Info}) ->
  maps:get(meta, Info, undefined).

'clojerl.IMeta.with_meta'(#?TYPE{name = ?M, info = Info} = List, Metadata) ->
  List#?TYPE{info = Info#{meta => Metadata}}.

'clojerl.ISeq.first'(#?TYPE{name = ?M, data = Fn}) ->
  Seq = clj_core:invoke(Fn, []),
  clj_core:first(Seq).

'clojerl.ISeq.next'(#?TYPE{name = ?M, data = Fn}) ->
  Seq = clj_core:invoke(Fn, []),
  clj_core:next(Seq).

'clojerl.ISeq.more'(#?TYPE{name = ?M, data = Fn}) ->
  Seq = clj_core:invoke(Fn, []),
  clj_core:rest(Seq).

'clojerl.ISequential.noop'(_) -> ok.

'clojerl.Seqable.seq'(#?TYPE{name = ?M, data = Fn}) ->
  case clj_core:invoke(Fn, []) of
    undefined -> undefined;
    Seq -> 'clojerl.Cons':new(clj_core:first(Seq),clj_core:next(Seq))
  end.

'clojerl.Stringable.str'(#?TYPE{name = ?M, data = Fn}) ->
  {uniq, Uniq} = erlang:fun_info(Fn, uniq),
  UniqBin = clj_core:str(Uniq),
  <<"#<clojerl.LazySeq@", UniqBin/binary, ">">>.
