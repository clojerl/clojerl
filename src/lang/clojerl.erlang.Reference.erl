-module('clojerl.erlang.Reference').

-behaviour('clojerl.Stringable').
-behaviour('clojerl.IHash').

-export(['clojerl.Stringable.str'/1]).
-export(['clojerl.IHash.hash'/1]).

%% clojerl.Stringable

'clojerl.Stringable.str'(Ref) when is_reference(Ref) ->
  <<"#Ref<", RefBin/binary>> = list_to_binary(erlang:ref_to_list(Ref)),
  <<"#<Ref ", RefBin/binary>>.

%% clojerl.IHash

'clojerl.IHash.hash'(Ref) when is_reference(Ref) ->
  erlang:phash2(Ref).
