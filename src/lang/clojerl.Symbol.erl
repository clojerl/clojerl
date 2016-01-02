-module('clojerl.Symbol').

-export([
         new/1,
         new/2
        ]).

-type type() :: {?MODULE, #{ns => binary() | undefined,
                            name => binary()}}.

-spec new(binary()) -> type().
new(Name) when is_binary(Name) ->
  new(undefined, Name).

-spec new(binary() | undefined, binary()) -> type().
new(Namespace, Name) when is_binary(Namespace) orelse Namespace == undefined,
                          is_binary(Name) ->
  {?MODULE, #{ns => Namespace, name => Name}}.
