-module('erlang.util.Date').

-include("clojerl.hrl").

-behavior('clojerl.IHash').
-behavior('clojerl.IEquiv').

-export([?CONSTRUCTOR/1, to_erl/1]).

-export([hash/1]).
-export([equiv/2]).

-type erlang_date() :: {{integer(), integer(), integer()}, {integer(), integer(), integer()}}.
-type type() :: #?TYPE{data :: erlang_date()}.

-spec ?CONSTRUCTOR(erlang_date()) -> type().
?CONSTRUCTOR(Date) -> #?TYPE{data = Date}.

to_erl(#?TYPE{data = Date}) ->
    Date.

%% ------------------------------------------------------------------------------
%% Protocols
%% ------------------------------------------------------------------------------

hash(Str) ->
  erlang:phash2(Str).

equiv(#?TYPE{data = DateA}, #?TYPE{data = DateB}) ->
  DateA == DateB.
