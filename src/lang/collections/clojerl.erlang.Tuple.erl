-module('clojerl.erlang.Tuple').

-behavior('clojerl.Counted').
-behavior('clojerl.ISequential').
-behavior('clojerl.Seqable').
-behavior('clojerl.Stringable').

-export(['clojerl.Counted.count'/1]).
-export(['clojerl.ISequential.noop'/1]).
-export(['clojerl.Seqable.seq'/1]).
-export(['clojerl.Stringable.str'/1]).

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

'clojerl.Counted.count'(Tuple) -> tuple_size(Tuple).

'clojerl.ISequential.noop'(_) -> ok.

'clojerl.Seqable.seq'({}) -> undefined;
'clojerl.Seqable.seq'(Tuple) -> tuple_to_list(Tuple).

'clojerl.Stringable.str'(Tuple) when is_tuple(Tuple) ->
  Items = tuple_to_list(Tuple),
  ItemsStrs = lists:map(fun clj_core:str/1, Items),
  Strs = clj_utils:binary_join(ItemsStrs, <<", ">>),
  <<"#[", Strs/binary, "]">>.
