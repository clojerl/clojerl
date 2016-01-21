-module('clojerl.List').

-behaviour('clojerl.Counted').
-behaviour('clojerl.Stringable').
-behaviour('clojerl.Seqable').
-behaviour('clojerl.ISeq').
-behaviour('clojerl.ISequential').

-export([ new/1
        , to_list/1
        ]).

-export(['clojerl.Counted.count'/1]).
-export(['clojerl.Stringable.str'/1]).
-export(['clojerl.Seqable.seq'/1]).

-type type() :: {?MODULE, list()}.

-spec new(list()) -> type().
new(Items) when is_list(Items) ->
  {?MODULE, Items, #{}}.

-spec to_list(type()) -> list().
to_list({_, List, _}) -> List.

'clojerl.Counted.count'({_, List, _}) -> length(List).

'clojerl.Stringable.str'({'clojerl.List', [], _}) ->
  <<"()">>;
'clojerl.Stringable.str'({'clojerl.List', Items, _}) ->
  ItemsStrs = lists:map(fun clj_core:str/1, Items),
  Strs = clj_utils:binary_join(ItemsStrs, <<", ">>),
  <<"(", Strs/binary, ")">>.

'clojerl.Seqable.seq'({_, [], _}) -> undefined;
'clojerl.Seqable.seq'({_, Seq, _}) -> Seq.
