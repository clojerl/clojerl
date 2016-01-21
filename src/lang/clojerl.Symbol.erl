-module('clojerl.Symbol').

-behavior('clojerl.IMeta').
-behavior('clojerl.Named').
-behaviour('clojerl.Stringable').

-define(T, ?MODULE).

-export([new/1, new/2, to_atom/1]).

-export([ 'clojerl.Named.name'/1
        , 'clojerl.Named.namespace'/1
        ]).
-export([ 'clojerl.IMeta.meta'/1
        , 'clojerl.IMeta.with_meta'/2
        ]).
-export(['clojerl.Stringable.str'/1]).

-record(?T, { ns         :: binary() | undefined
            , name       :: binary()
            , info = #{} :: map()
            }).

-type type() :: #?T{}.

-spec new(binary()) -> type().
new(Name) when is_binary(Name) ->
  new(undefined, Name).

-spec new(binary() | undefined, binary()) -> type().
new(Namespace, Name) when is_binary(Namespace) orelse Namespace == undefined,
                          is_binary(Name) ->
  #?T{ns = Namespace, name = Name}.

-spec to_atom(type()) -> atom().
to_atom(#?T{ns = undefined, name = Name}) ->
  binary_to_atom(Name, utf8);
to_atom(#?T{ns = Ns, name = Name}) ->
  binary_to_atom(<<Ns/binary, "/", Name/binary>>, utf8).

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

'clojerl.Stringable.str'(#?T{ns = undefined, name = Name}) ->
  Name;
'clojerl.Stringable.str'(#?T{ns = Namespace, name = Name}) ->
  <<Namespace/binary, "/", Name/binary>>.

'clojerl.Named.name'(#?T{name = Name}) -> Name.

'clojerl.Named.namespace'(#?T{ns = Namespace}) -> Namespace.

'clojerl.IMeta.meta'(#?T{info = Info}) ->
  maps:get(meta, Info, undefined).

'clojerl.IMeta.with_meta'(#?T{info = Info} = Keyword, Metadata) ->
  Keyword#?T{info = Info#{meta => Metadata}}.
