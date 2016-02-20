-module('clojerl.erlang.List').

-behavior('clojerl.Counted').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IColl').
-behavior('clojerl.ISeq').
-behavior('clojerl.ISequential').
-behavior('clojerl.IStack').
-behavior('clojerl.Seqable').
-behavior('clojerl.Stringable').

-export(['clojerl.Counted.count'/1]).
-export(['clojerl.IEquiv.equiv'/2]).
-export([ 'clojerl.IColl.cons'/2
        , 'clojerl.IColl.empty'/1
        ]).
-export([ 'clojerl.ISeq.first'/1
        , 'clojerl.ISeq.more'/1
        , 'clojerl.ISeq.next'/1
        ]).
-export(['clojerl.ISequential.noop'/1]).
-export([ 'clojerl.IStack.peek'/1
        , 'clojerl.IStack.pop'/1
        ]).
-export(['clojerl.Seqable.seq'/1]).
-export(['clojerl.Stringable.str'/1]).

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

'clojerl.Counted.count'(Items) -> length(Items).

'clojerl.Stringable.str'([]) ->
  <<"()">>;
'clojerl.Stringable.str'(Items) ->
  ItemsStrs = lists:map(fun clj_core:str/1, Items),
  Strs = clj_utils:binary_join(ItemsStrs, <<" ">>),
  <<"(", Strs/binary, ")">>.

'clojerl.Seqable.seq'([]) -> undefined;
'clojerl.Seqable.seq'(List) -> List.

'clojerl.ISeq.first'([]) -> undefined;
'clojerl.ISeq.first'([First | _]) -> First.

'clojerl.ISeq.more'([]) -> undefined;
'clojerl.ISeq.more'([_ | Rest]) -> Rest.

'clojerl.ISeq.next'([]) -> undefined;
'clojerl.ISeq.next'([_ | []]) -> undefined;
'clojerl.ISeq.next'([_ | Rest]) -> Rest.

'clojerl.ISequential.noop'(_) -> ok.

'clojerl.IStack.peek'([]) -> undefined;
'clojerl.IStack.peek'([X | _]) -> X.

'clojerl.IStack.pop'([]) -> [];
'clojerl.IStack.pop'([_ | Rest]) -> Rest.

'clojerl.IColl.cons'([], X) ->
  [X];
'clojerl.IColl.cons'(Items, X) ->
  [X | Items].

'clojerl.IColl.empty'(_) -> [].

'clojerl.IEquiv.equiv'(X, Y) when is_list(X), is_list(Y) ->
  case length(X) == length(Y) of
    true  -> do_equiv(X, Y);
    false -> false
  end;
'clojerl.IEquiv.equiv'(X, Y) ->
  case clj_core:'sequential?'(Y) of
    true  -> clj_core:equiv(Y, X);
    false -> false
  end.

do_equiv([], []) ->
  true;
do_equiv([X | TailX], [Y | TailY]) ->
  case clj_core:equiv(X, Y) of
    true  -> do_equiv(TailX, TailY);
    false -> false
  end.
