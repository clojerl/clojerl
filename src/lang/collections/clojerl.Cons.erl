-module('clojerl.Cons').

-include("clojerl.hrl").

-behavior('clojerl.Counted').
-behavior('clojerl.IMeta').
-behavior('clojerl.ISeq').
-behavior('clojerl.ISequential').
-behavior('clojerl.Seqable').
-behavior('clojerl.Stringable').

-export([new/2]).

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

-spec new(any(), any()) -> type().
new(First, More) ->
  #?TYPE{data = {First, More}}.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

'clojerl.Counted.count'(#?TYPE{name = ?M, data = {_, More}}) ->
  1 + clj_core:count(More).

'clojerl.IColl.cons'(#?TYPE{name = ?M} = Cons, X) -> new(X, Cons).

'clojerl.IColl.empty'(_) -> [].

'clojerl.IEquiv.equiv'( #?TYPE{name = ?M, data = {XFirst, XMore}}
                      , #?TYPE{name = ?M, data = {YFirst, YMore}}
                      ) ->
  clj_core:equiv(XFirst, YFirst) andalso clj_core:equiv(XMore, YMore);
'clojerl.IEquiv.equiv'(#?TYPE{name = ?M, data = X}, Y) ->
  case clj_core:'sequential?'(Y) of
    true  -> clj_core:equiv(clj_core:seq(X), clj_core:seq(Y));
    false -> false
  end.

'clojerl.IMeta.meta'(#?TYPE{name = ?M, info = Info}) ->
  maps:get(meta, Info, undefined).

'clojerl.IMeta.with_meta'(#?TYPE{name = ?M, info = Info} = List, Metadata) ->
  List#?TYPE{info = Info#{meta => Metadata}}.

'clojerl.ISeq.first'(#?TYPE{name = ?M, data = {First, _}}) -> First.

'clojerl.ISeq.next'(#?TYPE{name = ?M, data = {_, undefined}}) -> undefined;
'clojerl.ISeq.next'(#?TYPE{name = ?M, data = {_, More}}) -> More.

'clojerl.ISeq.more'(#?TYPE{name = ?M, data = {_, undefined}}) -> [];
'clojerl.ISeq.more'(#?TYPE{name = ?M, data = {_, More}}) -> More.

'clojerl.ISequential.noop'(_) -> ok.

'clojerl.Seqable.seq'(#?TYPE{name = ?M} = Cons) -> Cons.

'clojerl.Stringable.str'(#?TYPE{name = ?M} = Cons) ->
  clj_core:str(clj_core:seq_to_list(Cons)).
