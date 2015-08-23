-module('clojerl.Named').

-export([name/1, namespace/1]).

-type type() :: any().

-callback name(any()) -> binary() | 'clojerl.Symbol':type().
-callback namespace(any()) -> 'clojerl.Symbol':type().

-spec name(type()) -> any().
name(X) ->
  'clojerl.protocol':resolve('Named', name, [X]).

-spec namespace(type()) -> type().
namespace(X) ->
  'clojerl.protocol':resolve('Named', namespace, [X]).
