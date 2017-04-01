-module('clojerl.IMeta').

-clojure(true).
-protocol(true).

-export([meta/1, with_meta/2]).

-callback meta(any()) -> any().
-callback with_meta(T, 'clojerl.IMap':type()) -> T.

-spec meta(any()) -> any().
meta(X) ->
  clj_protocol:resolve(?MODULE, meta, X).

-spec with_meta(T, 'clojerl.IMap':type()) -> T.
with_meta(X, Meta) ->
  clj_protocol:resolve(?MODULE, with_meta, X, Meta).
