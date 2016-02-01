-module('clojerl.List').

-include("clojerl.hrl").

-behavior('clojerl.Counted').
-behavior('clojerl.Stringable').
-behavior('clojerl.Seqable').
-behavior('clojerl.ISequential').
-behavior('clojerl.ISeq').
-behavior('clojerl.IMeta').
-behavior('clojerl.IColl').

-export([new/1]).

-export(['clojerl.Counted.count'/1]).
-export(['clojerl.Stringable.str'/1]).
-export(['clojerl.Seqable.seq'/1]).
-export([ 'clojerl.ISeq.first'/1
        , 'clojerl.ISeq.next'/1
        , 'clojerl.ISeq.more'/1
        ]).
-export(['clojerl.ISequential.noop'/1]).
-export([ 'clojerl.IMeta.meta'/1
        , 'clojerl.IMeta.with_meta'/2
        ]).
-export([ 'clojerl.IColl.count'/1
        , 'clojerl.IColl.cons'/2
        , 'clojerl.IColl.empty'/1
        , 'clojerl.IColl.equiv'/2
        ]).

-type type() :: #?TYPE{}.

-spec new(list()) -> type().
new(Items) when is_list(Items) ->
  #?TYPE{data = Items}.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

'clojerl.Counted.count'(#?TYPE{name = ?M, data = Items}) -> length(Items).

'clojerl.Stringable.str'(#?TYPE{name = ?M, data = []}) ->
  <<"()">>;
'clojerl.Stringable.str'(#?TYPE{name = ?M, data = Items}) ->
  ItemsStrs = lists:map(fun clj_core:str/1, Items),
  Strs = clj_utils:binary_join(ItemsStrs, <<" ">>),
  <<"(", Strs/binary, ")">>.

'clojerl.Seqable.seq'(#?TYPE{name = ?M, data = []}) -> undefined;
'clojerl.Seqable.seq'(#?TYPE{name = ?M, data = Seq}) -> Seq.

'clojerl.ISeq.first'(#?TYPE{name = ?M, data = []}) -> undefined;
'clojerl.ISeq.first'(#?TYPE{name = ?M, data = [First | _]}) -> First.

'clojerl.ISeq.next'(#?TYPE{name = ?M, data = []}) -> undefined;
'clojerl.ISeq.next'(#?TYPE{name = ?M, data = [_ | []]}) -> undefined;
'clojerl.ISeq.next'(#?TYPE{name = ?M, data = [_ | Rest]} = List) ->
  List#?TYPE{name = ?M, data = Rest}.

'clojerl.ISeq.more'(#?TYPE{name = ?M, data = []}) -> undefined;
'clojerl.ISeq.more'(#?TYPE{name = ?M, data = [_ | Rest]} = List) ->
  List#?TYPE{data = Rest}.

'clojerl.ISequential.noop'(_) -> ok.

'clojerl.IMeta.meta'(#?TYPE{name = ?M, info = Info}) ->
  maps:get(meta, Info, undefined).

'clojerl.IMeta.with_meta'(#?TYPE{name = ?M, info = Info} = List, Metadata) ->
  List#?TYPE{info = Info#{meta => Metadata}}.

'clojerl.IColl.count'(#?TYPE{name = ?M, data = Items}) -> length(Items).

'clojerl.IColl.cons'(#?TYPE{name = ?M, data = []} = List, X) ->
  List#?TYPE{data = [X]};
'clojerl.IColl.cons'(#?TYPE{name = ?M, data = Items} = List, X) ->
  List#?TYPE{data = [X | Items]}.

'clojerl.IColl.empty'(_) -> new([]).

'clojerl.IColl.equiv'(X, X) -> true;
'clojerl.IColl.equiv'(_, _) -> false.
