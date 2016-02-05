-module('clojerl.Symbol').

-include("clojerl.hrl").

-behavior('clojerl.IEquiv').
-behavior('clojerl.IMeta').
-behavior('clojerl.Named').
-behavior('clojerl.Stringable').

-export([new/1, new/2, to_atom/1]).

-export([ 'clojerl.Named.name'/1
        , 'clojerl.Named.namespace'/1
        ]).
-export(['clojerl.IEquiv.equiv'/2]).
-export([ 'clojerl.IMeta.meta'/1
        , 'clojerl.IMeta.with_meta'/2
        ]).
-export(['clojerl.Stringable.str'/1]).

-type type() :: #?TYPE{}.

-spec new(binary()) -> type().
new(Name) when is_binary(Name) ->
  new(undefined, Name).

-spec new(binary() | undefined, binary()) -> type().
new(Namespace, Name) when is_binary(Namespace) orelse Namespace == undefined,
                          is_binary(Name) ->
  #?TYPE{data = {Namespace, Name}}.

-spec to_atom(type()) -> atom().
to_atom(#?TYPE{name = ?M, data = {undefined, Name}}) ->
  binary_to_atom(Name, utf8);
to_atom(#?TYPE{name = ?M, data = {Ns, Name}}) ->
  binary_to_atom(<<Ns/binary, "/", Name/binary>>, utf8).

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
'clojerl.IEquiv.equiv'(#?TYPE{name = ?M}, #?TYPE{name = ?M}) ->
  false.
