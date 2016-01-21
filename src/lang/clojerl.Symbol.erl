-module('clojerl.Symbol').

-behaviour('clojerl.Stringable').

-export([new/1, new/2, to_atom/1]).
-export(['clojerl.Stringable.str'/1]).

-type type() :: {?MODULE, #{ns => binary() | undefined,
                            name => binary()}}.

-spec new(binary()) -> type().
new(Name) when is_binary(Name) ->
  new(undefined, Name).

-spec new(binary() | undefined, binary()) -> type().
new(Namespace, Name) when is_binary(Namespace) orelse Namespace == undefined,
                          is_binary(Name) ->
  {?MODULE, #{ns => Namespace, name => Name}}.

-spec to_atom(type()) -> atom().
to_atom({?MODULE, #{ns := undefined, name := Name}}) ->
  binary_to_atom(Name, utf8);
to_atom({?MODULE, #{ns := Ns, name := Name}}) ->
  binary_to_atom(<<Ns/binary, "/", Name/binary>>, utf8).

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

'clojerl.Stringable.str'({ 'clojerl.Symbol'
                         , #{ns := undefined, name := Name}}) ->
  Name;
'clojerl.Stringable.str'({ 'clojerl.Symbol'
                         , #{ns := Namespace, name := Name}}) ->
  <<Namespace/binary, "/", Name/binary>>.
