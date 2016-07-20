-module('clojerl.erlang.Reference').

-behaviour('clojerl.Stringable').
-behaviour('clojerl.IHash').

-export([str/1]).
-export([hash/1]).

%% clojerl.Stringable

str(Ref) when is_reference(Ref) ->
  <<"#Ref<", RefBin/binary>> = list_to_binary(erlang:ref_to_list(Ref)),
  <<"#<Ref ", RefBin/binary>>.

%% clojerl.IHash

hash(Ref) when is_reference(Ref) ->
  erlang:phash2(Ref).
