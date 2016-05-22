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

-export(['clojerl.Counted.count'/1]).
-export([ 'clojerl.IColl.cons'/2
        , 'clojerl.IColl.empty'/1
        ]).
-export(['clojerl.IEquiv.equiv'/2]).
-export(['clojerl.IHash.hash'/1]).
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
  case clj_core:invoke(Fn, []) of
    undefined -> 0;
    Seq       -> clj_core:count(Seq)
  end.

'clojerl.IColl.cons'(#?TYPE{name = ?M} = LazySeq, X) ->
  'clojerl.Cons':new(X, LazySeq).

'clojerl.IColl.empty'(_) -> [].

'clojerl.IEquiv.equiv'( #?TYPE{name = ?M, data = X}
                      , #?TYPE{name = ?M, data = Y}
                      ) ->
  clj_core:equiv(X, Y);
'clojerl.IEquiv.equiv'(#?TYPE{name = ?M} = LazySeq, Y) ->
  case clj_core:'sequential?'(Y) of
    true  -> clj_core:equiv(clj_core:seq_to_list(LazySeq), clj_core:seq(Y));
    false -> false
  end.

'clojerl.IHash.hash'(#?TYPE{name = ?M} = LazySeq) ->
  clj_murmur3:ordered(LazySeq).

'clojerl.IMeta.meta'(#?TYPE{name = ?M, info = Info}) ->
  maps:get(meta, Info, undefined).

'clojerl.IMeta.with_meta'(#?TYPE{name = ?M, info = Info} = List, Metadata) ->
  List#?TYPE{info = Info#{meta => Metadata}}.

'clojerl.ISeq.first'(#?TYPE{name = ?M, data = Fn}) ->
  Seq = clj_core:invoke(Fn, []),
  clj_core:first(Seq).

'clojerl.ISeq.next'(#?TYPE{name = ?M, data = Fn}) ->
  case clj_core:invoke(Fn, []) of
    undefined -> [];
    Seq -> clj_core:next(Seq)
  end.

'clojerl.ISeq.more'(#?TYPE{name = ?M, data = Fn}) ->
  Seq = clj_core:invoke(Fn, []),
  clj_core:rest(Seq).

'clojerl.ISequential.noop'(_) -> ok.

'clojerl.Seqable.seq'(#?TYPE{name = ?M, data = Fn}) ->
  case clj_core:invoke(Fn, []) of
    undefined ->
      undefined;
    #?TYPE{name = ?M} = LazySeq ->
      'clojerl.Seqable.seq'(LazySeq);
    Seq ->
      Seq
  end.

'clojerl.Stringable.str'(#?TYPE{name = ?M, data = Fn}) ->
  {uniq, Uniq} = erlang:fun_info(Fn, uniq),
  UniqBin = clj_core:str(Uniq),
  <<"#<clojerl.LazySeq@", UniqBin/binary, ">">>.
