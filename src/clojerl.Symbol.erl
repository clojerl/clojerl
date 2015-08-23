-module('clojerl.Symbol').

-export([
         new/1,
         new/2,
         namespace/1,
         name/1,
         is/1
        ]).

-include("include/clj_types.hrl").

-spec new(atom()) -> symbol().
new(Name) ->
  new('_', Name).

-spec new(atom(), atom()) -> symbol().
new(Namespace, Name) ->
  {symbol, #{ns => Namespace, name => Name}}.

-spec namespace(symbol()) -> binary().
namespace({symbol, #{ns := Namespace}}) ->
  Namespace.

-spec name(symbol()) -> binary().
name({symbol, #{name := Name}}) ->
  Name.

-spec is(sexpr()) -> boolean().
is(X) -> clj_core:type(X) == ?MODULE.
