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
        , timestamp/1
        ]).

-export([hash/1]).
-export([equiv/2]).

%% EPOCH = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}).
-define(EPOCH, 62167219200).

-type type() :: #{ ?TYPE => ?M
                 , date  => calendar:datetime()
                 }.

-spec ?CONSTRUCTOR(calendar:datetime()) -> type().
?CONSTRUCTOR(Timestamp) when is_integer(Timestamp), -?EPOCH =< Timestamp ->
  Datetime = calendar:gregorian_seconds_to_datetime(?EPOCH + Timestamp),
  ?CONSTRUCTOR(Datetime);
?CONSTRUCTOR({{Y, MM, D}, {H, M, S}} = Date)
  when is_integer(Y), is_integer(MM), is_integer(D),
       is_integer(H), is_integer(M), is_integer(S) ->
  #{?TYPE => ?M, date => Date}.

-spec year(type()) -> integer().
year(#{?TYPE := ?M, date := {{Y, _, _}, _}}) -> Y.

-spec month(type()) -> integer().
month(#{?TYPE := ?M, date := {{_, M, _}, _}}) -> M.

-spec day(type()) -> integer().
day(#{?TYPE := ?M, date := {{_, _, D}, _}}) -> D.

-spec hours(type()) -> integer().
hours(#{?TYPE := ?M, date := {_, {H, _, _}}}) -> H.

-spec minutes(type()) -> integer().
minutes(#{?TYPE := ?M, date := {_, {_, M, _}}}) -> M.

-spec seconds(type()) -> integer().
seconds(#{?TYPE := ?M, date := {_, {_, _, S}}}) -> S.

timestamp(#{?TYPE := ?M, date := DateTime}) ->
  calendar:datetime_to_gregorian_seconds(DateTime) - ?EPOCH.

%% -----------------------------------------------------------------------------
%% Protocols
%% -----------------------------------------------------------------------------

hash(Date) -> erlang:phash2(Date).

equiv(#{?TYPE := ?M, date := Date}, #{?TYPE := ?M, date := Date}) -> true;
equiv(_ , _) -> false.
