-module(clj_symbol).

-export([
         new/1,
         new/2,
         is/1
        ]).

-include("include/clj_types.hrl").

-spec new(atom()) -> symbol().
new(Name) ->
  new('_', Name).

-spec new(atom(), atom()) -> symbol().
new(Namespace, Name) ->
  {symbol, #{ns => Namespace, name => Name}}.

-spec is(sexpr()) -> boolean().
is(X) -> clj_utils:type(X) == symbol.
