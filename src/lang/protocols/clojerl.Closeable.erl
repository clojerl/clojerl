-module('clojerl.Closeable').

-export([close/1]).

-type type() :: any().

-callback 'clojerl.Closeable.close'(C :: type()) -> undefined.

-spec close(type()) -> undefined.
close(X) ->
  'clojerl.protocol':resolve(?MODULE, close, [X]).
