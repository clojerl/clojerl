-module('clojerl.IColl').

-clojure(true).
-protocol(true).

-export([cons/2, empty/1]).

-type type() :: any().

-callback 'cons'(IColl :: type(), Item :: any()) -> type().
-callback empty(IColl :: type()) -> any().

-spec cons(type(), any()) -> type().
cons(Coll, Item) ->
  clj_protocol:resolve(?MODULE, cons, Coll, Item).

-spec empty(type()) -> any().
empty(Coll) ->
  clj_protocol:resolve(?MODULE, empty, Coll).
