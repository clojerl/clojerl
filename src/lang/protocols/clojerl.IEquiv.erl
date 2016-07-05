-module('clojerl.IEquiv').

-export([equiv/2]).

-type type() :: any().

-callback equiv(type(), type()) -> boolean().

-spec equiv(type(), type()) -> type().
equiv(X, Y) ->
  'clojerl.protocol':resolve(?MODULE, equiv, [X, Y]).
