-module('clojerl.Keyword').

-export([
         new/1,
         new/2,
         is/1
        ]).

-include("include/clj_types.hrl").

-spec new(atom()) -> keyword().
new(Name) ->
  new('_', Name).

-spec new(atom(), atom()) -> keyword().
new(Namespace, Name) ->
  {?MODULE, #{ns => Namespace, name => Name}}.

-spec is(sexpr()) -> boolean().
is(X) -> clj_core:type(X) == ?MODULE.
