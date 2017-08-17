-module('clojerl.INamed').

-include("clojerl.hrl").

-clojure(true).
-protocol(true).

-export([name/1, namespace/1]).

-callback name(any()) -> binary().
-callback namespace(any()) -> binary().

-spec name(any()) -> binary() | ?NIL.
name(X) ->
  clj_protocol:resolve(?MODULE, name, X).

-spec namespace(any()) -> binary() | ?NIL.
namespace(X) ->
  clj_protocol:resolve(?MODULE, namespace, X).
