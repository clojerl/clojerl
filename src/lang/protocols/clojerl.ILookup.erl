-module('clojerl.ILookup').

-export([get/2, get/3]).

-type type() ::  any().
-export_type([type/0]).

-callback meta(any()) -> any().

-spec get(any(), any()) -> any().
get(X, Key) ->
  'clojerl.protocol':resolve(?MODULE, meta, [X, Key]).

-spec get(any(), any(), any()) -> any().
get(X, Key, NotFound) ->
  'clojerl.protocol':resolve(?MODULE, meta, [X, Key, NotFound]).
