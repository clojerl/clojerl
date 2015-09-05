-module('clojerl.Keyword').

-export([
         new/1,
         new/2,
         is/1
        ]).


-type type() :: {?MODULE, #{ns => binary() | undefined,
                            name => binary()}}.

-spec new(binary()) -> 'clojerl.Keyword':type().
new(Name) ->
  new(undefined, Name).

-spec new(binary(), binary()) -> 'clojerl.Keyword':type().
new(Namespace, Name) ->
  {?MODULE, #{ns => Namespace, name => Name}}.

-spec is(any()) -> boolean().
is(X) -> clj_core:type(X) == ?MODULE.
