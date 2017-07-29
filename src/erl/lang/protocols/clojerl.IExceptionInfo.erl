-module('clojerl.IExceptionInfo').

-clojure(true).
-protocol(true).

-export([data/1, cause/1]).

-type type() :: any().

-callback data(A :: type()) -> any().
-callback cause(A :: type()) -> any().

-spec data(type()) -> any().
data(ExInfo) ->
  clj_protocol:resolve(?MODULE, data, ExInfo).

-spec cause(type()) -> any().
cause(ExInfo) ->
  clj_protocol:resolve(?MODULE, cause, ExInfo).
