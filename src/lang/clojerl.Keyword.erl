-module('clojerl.Keyword').

-behavior('clojerl.IFn').
-behavior('clojerl.Named').
-behavior('clojerl.Stringable').

-export([new/1, new/2]).

-export([ 'clojerl.Named.name'/1
        , 'clojerl.Named.namespace'/1
        ]).
-export(['clojerl.Stringable.str'/1]).
-export(['clojerl.IFn.invoke'/2]).

-type type() :: atom().

-spec new(binary()) -> type().
new(Name) ->
  binary_to_atom(Name, utf8).

-spec new(binary(), binary()) -> type().
new(Namespace, Name) ->
  binary_to_atom(<<Namespace/binary, "/", Name/binary>>, utf8).

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

'clojerl.Stringable.str'(Keyword) ->
  KeywordBin = atom_to_binary(Keyword, utf8),
  <<":", KeywordBin/binary>>.

'clojerl.Named.name'(Keyword) ->
  KeywordBin = atom_to_binary(Keyword, utf8),
  case binary:split(KeywordBin, <<"/">>) of
    [_] -> KeywordBin;
    [_, Name] -> Name
  end.

'clojerl.Named.namespace'(Keyword) ->
  KeywordBin = atom_to_binary(Keyword, utf8),
  case binary:split(KeywordBin, <<"/">>) of
    [_] -> undefined;
    [Namespace, _] -> Namespace
  end.

'clojerl.IFn.invoke'(Keyword, [Map]) ->
  clj_core:get(Map, Keyword);
'clojerl.IFn.invoke'(Keyword, [Map, NotFound]) ->
  clj_core:get(Map, Keyword, NotFound);
'clojerl.IFn.invoke'(_, Args) ->
  CountBin = integer_to_binary(length(Args)),
  throw(<<"Wrong number of args for keyword, got: ", CountBin/binary>>).
