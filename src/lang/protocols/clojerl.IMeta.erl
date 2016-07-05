-module('clojerl.IMeta').

-export([meta/1, with_meta/2]).

-type type() ::  any().

-callback meta(any()) -> any().
-callback with_meta(any(), 'clojerl.Map':type()) -> any().

-spec meta(type()) -> any().
meta(X) ->
  'clojerl.protocol':resolve(?MODULE, meta, [X]).

-spec with_meta(type(), 'clojerl.Map':type()) -> type().
with_meta(X, Meta) ->
  'clojerl.protocol':resolve(?MODULE, with_meta, [X, Meta]).
