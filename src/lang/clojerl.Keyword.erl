-module('clojerl.Keyword').

-behaviour('clojerl.Stringable').

-export([new/1, new/2]).
-export(['clojerl.Stringable.str'/1]).

-type type() :: {?MODULE, #{ns => binary() | undefined,
                            name => binary()}}.

-spec new(binary()) -> 'clojerl.Keyword':type().
new(Name) ->
  new(undefined, Name).

-spec new(binary(), binary()) -> 'clojerl.Keyword':type().
new(Namespace, Name) ->
  {?MODULE, #{ns => Namespace, name => Name}}.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

'clojerl.Stringable.str'({'clojerl.Keyword', #{ns := undefined, name := Name}}) ->
  <<":", Name/binary>>;
'clojerl.Stringable.str'({'clojerl.Keyword', #{ns := Namespace, name := Name}}) ->
  <<":", Namespace/binary, "/", Name/binary>>.
