-module('clojerl.Symbol').

-include("clojerl.hrl").

-behavior('clojerl.IEquiv').
-behavior('clojerl.IHash').
-behavior('clojerl.IMeta').
-behavior('clojerl.Named').
-behavior('clojerl.Stringable').

-export([new/1, new/2]).

-export([ 'clojerl.Named.name'/1
        , 'clojerl.Named.namespace'/1
        ]).
-export(['clojerl.IEquiv.equiv'/2]).
-export(['clojerl.IHash.hash'/1]).
-export([ 'clojerl.IMeta.meta'/1
        , 'clojerl.IMeta.with_meta'/2
        ]).
-export(['clojerl.Stringable.str'/1]).

-type type() :: #?TYPE{data :: {undefined | binary(), binary()}}.

-spec new(binary()) -> type().
new(Name) when is_binary(Name) ->
  new(undefined, Name).

-spec new(binary() | undefined, binary()) -> type().
new(Namespace, Name) when is_binary(Namespace) orelse Namespace == undefined,
                          is_binary(Name) ->
  #?TYPE{data = {Namespace, Name}}.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

'clojerl.Stringable.str'(#?TYPE{name = ?M, data = {undefined, Name}}) ->
  Name;
'clojerl.Stringable.str'(#?TYPE{name = ?M, data = {Namespace, Name}}) ->
  <<Namespace/binary, "/", Name/binary>>.

'clojerl.Named.name'(#?TYPE{name = ?M, data = {_, Name}}) -> Name.

'clojerl.Named.namespace'(#?TYPE{name = ?M, data = {Namespace, _}}) ->
  Namespace.

'clojerl.IMeta.meta'(#?TYPE{name = ?M, info = Info}) ->
  maps:get(meta, Info, undefined).

'clojerl.IMeta.with_meta'( #?TYPE{name = ?M, info = Info} = Keyword
                         , Metadata
                         ) ->
  Keyword#?TYPE{info = Info#{meta => Metadata}}.

'clojerl.IEquiv.equiv'( #?TYPE{name = ?M, data = X}
                      , #?TYPE{name = ?M, data = X}
                      ) ->
  true;
'clojerl.IEquiv.equiv'(_, _) ->
  false.

'clojerl.IHash.hash'(#?TYPE{name = ?M, data = Data}) ->
  erlang:phash2(Data).
