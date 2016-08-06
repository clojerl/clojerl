-module('erlang.util.Regex').

-include("clojerl.hrl").

-behavior('clojerl.IHash').
-behavior('clojerl.Stringable').

-export([?CONSTRUCTOR/1, run/3]).
-export([hash/1]).
-export([str/1]).

-type type() :: #?TYPE{data :: {binary(), tuple()}}.

-spec ?CONSTRUCTOR(binary()) -> type().
?CONSTRUCTOR(Pattern) when is_binary(Pattern) ->
  {ok, Regex} = re:compile(Pattern),
  #?TYPE{data = {Pattern, Regex}}.

-spec run(type(), binary(), [term()]) ->
  {match, term()} | match | nomatch | {error, term()}.
run(#?TYPE{name = ?M, data = {_, Regex}}, Str, Opts) ->
  re:run(Str, Regex, Opts).

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

hash(#?TYPE{name = ?M, data = {Pattern, _}}) -> erlang:phash2(Pattern).

str(#?TYPE{name = ?M, data = {Pattern, _}}) -> <<"#\"", Pattern/binary, "\"">>.
