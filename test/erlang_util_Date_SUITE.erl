-module(erlang_util_Date_SUITE).

-include("clojerl.hrl").
-include("clj_test_utils.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).

-export([ new/1
        , equiv/1
        , hash/1
        ]).

-spec all() -> [atom()].
all() -> clj_test_utils:all(?MODULE).

-spec init_per_suite(config()) -> config().
init_per_suite(Config) -> clj_test_utils:init_per_suite(Config).

-spec end_per_suite(config()) -> config().
end_per_suite(Config) -> Config.

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------

-spec new(config()) -> result().
new(_Config) ->
  Now = erlang:localtime(),
  {{Year, Month, Day}, {Hours, Minutes, Seconds}} = Now,
  Date = 'erlang.util.Date':?CONSTRUCTOR(Now),

  Year    = 'erlang.util.Date':year(Date),
  Month   = 'erlang.util.Date':month(Date),
  Day     = 'erlang.util.Date':day(Date),
  Hours   = 'erlang.util.Date':hours(Date),
  Minutes = 'erlang.util.Date':minutes(Date),
  Seconds = 'erlang.util.Date':seconds(Date),

  Epoch     = {{1970, 1, 1}, {0, 0, 0}},
  EpochDate = 'erlang.util.Date':?CONSTRUCTOR(Epoch),
  0         = 'erlang.util.Date':timestamp(EpochDate),

  ct:comment("Provide a timestamp"),
  EpochDate = 'erlang.util.Date':?CONSTRUCTOR(0),

  {comments, ""}.

-spec hash(config()) -> result().
hash(_Config) ->
  Now = erlang:localtime(),
  Seconds = calendar:datetime_to_gregorian_seconds(Now),
  NowTomorrow = calendar:gregorian_seconds_to_datetime(Seconds + 3600 * 24),
  Date1 = 'erlang.util.Date':?CONSTRUCTOR(Now),
  Date2 = 'erlang.util.Date':?CONSTRUCTOR(Now),
  Date3 = 'erlang.util.Date':?CONSTRUCTOR(NowTomorrow),

  true  = 'clojerl.IHash':hash(Date1) == 'clojerl.IHash':hash(Date2),
  false = 'clojerl.IHash':hash(Date3) == 'clojerl.IHash':hash(Date2),

  {comments, ""}.

-spec equiv(config()) -> result().
equiv(_Config) ->
  Now = erlang:localtime(),
  Seconds = calendar:datetime_to_gregorian_seconds(Now),
  NowTomorrow = calendar:gregorian_seconds_to_datetime(Seconds + 3600 * 24),
  Date1 = 'erlang.util.Date':?CONSTRUCTOR(Now),
  Date2 = 'erlang.util.Date':?CONSTRUCTOR(Now),
  Date3 = 'erlang.util.Date':?CONSTRUCTOR(NowTomorrow),

  true  = 'clojerl.IEquiv':equiv(Date1, Date2),
  false = 'clojerl.IEquiv':equiv(Date1, Date3),

  {comments, ""}.
