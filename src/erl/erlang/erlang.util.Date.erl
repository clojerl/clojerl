-module('erlang.util.Date').

-include("clojerl.hrl").

-behavior('clojerl.IHash').
-behavior('clojerl.IEquiv').

-export([?CONSTRUCTOR/1, to_erl/1]).

-export([hash/1]).
-export([equiv/2]).

-type type() :: #?TYPE{data :: calendar:datetime()}.

-spec ?CONSTRUCTOR(calendar:datetime()) -> type().
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
