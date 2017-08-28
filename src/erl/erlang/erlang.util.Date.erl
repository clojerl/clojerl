-module('erlang.util.Date').

-include("clojerl.hrl").

-behavior('clojerl.IHash').
-behavior('clojerl.IEquiv').

-export([ ?CONSTRUCTOR/1
        , year/1
        , month/1
        , day/1
        , hours/1
        , minutes/1
        , seconds/1
        ]).

-export([hash/1]).
-export([equiv/2]).

-type type() :: #?TYPE{data :: calendar:datetime()}.

-spec ?CONSTRUCTOR(calendar:datetime()) -> type().
?CONSTRUCTOR(Date) -> #?TYPE{data = Date}.

-spec year(type()) -> integer().
year(#?TYPE{name = ?M, data = {{Y, _, _}, _}}) -> Y.

-spec month(type()) -> integer().
month(#?TYPE{name = ?M, data = {{_, M, _}, _}}) -> M.

-spec day(type()) -> integer().
day(#?TYPE{name = ?M, data = {{_, _, D}, _}}) -> D.

-spec hours(type()) -> integer().
hours(#?TYPE{name = ?M, data = {_, {H, _, _}}}) -> H.

-spec minutes(type()) -> integer().
minutes(#?TYPE{name = ?M, data = {_, {_, M, _}}}) -> M.

-spec seconds(type()) -> integer().
seconds(#?TYPE{name = ?M, data = {_, {_, _, S}}}) -> S.

%% -----------------------------------------------------------------------------
%% Protocols
%% -----------------------------------------------------------------------------

hash(Date) ->
  erlang:phash2(Date).

equiv(#?TYPE{data = Date}, #?TYPE{data = Date}) -> true;
equiv(_ , _) -> false.
