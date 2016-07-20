-module('erlang.util.UUID').

-include("clojerl.hrl").

-behaviour('clojerl.Stringable').

-export([new/1]).

-export([str/1]).

-type type() :: #?TYPE{data :: binary()}.

-define(UUID_REGEX, "^[a-fA-F0-9]{8}-"
                    "[a-fA-F0-9]{4}-"
                    "[a-fA-F0-9]{4}-"
                    "[a-fA-F0-9]{4}-"
                    "[a-fA-F0-9]{12}$"
       ).

-spec new(binary()) -> type().
new(UUID) when is_binary(UUID) ->
  case is_uuid(UUID) of
    false -> error(<<"Invalid UUID: ", UUID/binary>>);
    true  -> #?TYPE{data = UUID}
  end.

-spec is_uuid(binary()) -> boolean().
is_uuid(MaybeUUID) ->
  re:run(MaybeUUID, ?UUID_REGEX) =/= nomatch.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

str(#?TYPE{name = ?M, data = UUID}) -> UUID.
