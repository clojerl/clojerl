-module('clojerl.Symbol').

-export([
         new/1,
         new/2,
         is/1
        ]).

-include("include/clj_types.hrl").

-spec new(binary()) -> symbol().
new(Name) ->
  new(undefined, Name).

-spec new(binary(), binary()) -> symbol().
new(Namespace, Name) ->
  {?MODULE, #{ns => Namespace, name => Name}}.

-spec is(sexpr()) -> boolean().
is(X) -> clj_core:type(X) == ?MODULE.
