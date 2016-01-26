-module('clojerl.Named').

-export([name/1, namespace/1]).

-type type() :: any().

-callback 'clojerl.Named.name'(any()) -> binary().
-callback 'clojerl.Named.namespace'(any()) -> binary().

-spec name(type()) -> binary().
name(X) ->
  'clojerl.protocol':resolve(?MODULE, name, [X]).

-spec namespace(type()) -> binary().
namespace(X) ->
  'clojerl.protocol':resolve(?MODULE, namespace, [X]).
