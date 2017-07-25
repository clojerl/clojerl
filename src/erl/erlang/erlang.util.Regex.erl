-module('erlang.util.Regex').

-include("clojerl.hrl").

-behavior('clojerl.IHash').
-behavior('clojerl.Stringable').

-export([ ?CONSTRUCTOR/1
        , run/3
        , replace/3
        , replace/4
        , quote/1
        , split/3
        ]).
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

-spec replace(type(), binary(), binary()) -> binary().
replace(#?TYPE{name = ?M} = Regex, Str, Replacement) ->
  replace(Regex, Str, Replacement, [global, {return, binary}]).

-spec replace(type(), binary(), binary(), [term()]) -> binary().
replace(Regex, Str, Replacement, Opts) when is_binary(Regex) ->
  replace(?CONSTRUCTOR(Regex), Str, Replacement, Opts);
replace(#?TYPE{name = ?M, data = {_, Regex}}, Str, Replacement, Opts) ->
  re:replace(Str, Regex, Replacement, [{return, binary} | Opts]).

-spec quote(binary()) -> binary().
quote(Regex) when is_binary(Regex) ->
  do_quote(Regex, <<>>).

-spec split(type(), binary(), [term()]) -> [binary()].
split(Regex, Str, Opts) when is_binary(Regex) ->
  split(?CONSTRUCTOR(Regex), Str, Opts);
split(#?TYPE{name = ?M, data = {_, Regex}}, Str, Opts) ->
  re:split(Str, Regex, Opts).

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

hash(#?TYPE{name = ?M, data = {Pattern, _}}) -> erlang:phash2(Pattern).

str(#?TYPE{name = ?M, data = {Pattern, _}}) -> <<"#\"", Pattern/binary, "\"">>.

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

-spec do_quote(binary(), binary()) -> binary().
do_quote(<<>>, Acc) ->
  Acc;
do_quote(<<Ch/utf8, Rest/binary>>, Acc) ->
  NewCh = case Ch of
            $$  -> <<"\\$">>;
            $\\ -> <<"\\\\">>;
            _   -> <<Ch/utf8>>
          end,
  do_quote(Rest, <<Acc/binary, NewCh/binary>>).
