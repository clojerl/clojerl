-module('clojerl.List').

-include("clojerl.hrl").

-behavior('clojerl.Counted').
-behavior('clojerl.IColl').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IHash').
-behavior('clojerl.IMeta').
-behavior('clojerl.ISeq').
-behavior('clojerl.ISequential').
-behavior('clojerl.IStack').
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
-export([ 'clojerl.IStack.peek'/1
        , 'clojerl.IStack.pop'/1
        ]).
-export(['clojerl.Seqable.seq'/1]).
-export(['clojerl.Stringable.str'/1]).

-type type() :: #?TYPE{}.

-spec new(list()) -> type().
new(Items) when is_list(Items) ->
  #?TYPE{data = Items}.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

'clojerl.Counted.count'(#?TYPE{name = ?M, data = Items}) -> length(Items).

'clojerl.IColl.cons'(#?TYPE{name = ?M, data = []} = List, X) ->
  List#?TYPE{data = [X]};
'clojerl.IColl.cons'(#?TYPE{name = ?M, data = Items} = List, X) ->
  List#?TYPE{data = [X | Items]}.

'clojerl.IColl.empty'(_) -> new([]).

'clojerl.IEquiv.equiv'( #?TYPE{name = ?M, data = X}
                      , #?TYPE{name = ?M, data = Y}
                      ) ->
  clj_core:equiv(X, Y);
'clojerl.IEquiv.equiv'(#?TYPE{name = ?M, data = X}, Y) ->
  case clj_core:'sequential?'(Y) of
    true  -> clj_core:equiv(X, clj_core:seq(Y));
    false -> false
  end.

'clojerl.IHash.hash'(#?TYPE{name = ?M, data = X}) ->
  erlang:phash2(X).

'clojerl.IMeta.meta'(#?TYPE{name = ?M, info = Info}) ->
  maps:get(meta, Info, undefined).

'clojerl.IMeta.with_meta'(#?TYPE{name = ?M, info = Info} = List, Metadata) ->
  List#?TYPE{info = Info#{meta => Metadata}}.

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

'clojerl.IStack.peek'(#?TYPE{name = ?M, data = List}) ->
  clj_core:peek(List).

'clojerl.IStack.pop'(#?TYPE{name = ?M, data = []} = List) ->
  List;
'clojerl.IStack.pop'(#?TYPE{name = ?M, data = [_ | Rest]} = List) ->
  List#?TYPE{data = Rest}.

'clojerl.Seqable.seq'(#?TYPE{name = ?M, data = []}) -> undefined;
'clojerl.Seqable.seq'(#?TYPE{name = ?M, data = Seq}) -> Seq.

'clojerl.Stringable.str'(#?TYPE{name = ?M, data = []}) ->
  <<"()">>;
'clojerl.Stringable.str'(#?TYPE{name = ?M, data = Items}) ->
  ItemsStrs = lists:map(fun clj_core:str/1, Items),
  Strs = clj_utils:binary_join(ItemsStrs, <<" ">>),
  <<"(", Strs/binary, ")">>.
