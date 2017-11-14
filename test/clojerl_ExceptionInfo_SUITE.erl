-module(clojerl_ExceptionInfo_SUITE).

-include("clojerl.hrl").
-include("clj_test_utils.hrl").

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        ]).

-export([ equiv/1
        , fields/1
        , hash/1
        , str/1
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

-spec equiv(config()) -> result().
equiv(_Config) ->
  ct:comment("Only ex-info with the same message, data and cause are equiv"),
  ExInfo1 = 'clojerl.ExceptionInfo':?CONSTRUCTOR(<<"hello">>, data),
  ExInfo2 = 'clojerl.ExceptionInfo':?CONSTRUCTOR(<<"hello">>, data),

  true    = clj_rt:equiv(ExInfo1, ExInfo2),

  ExInfo3 = 'clojerl.ExceptionInfo':?CONSTRUCTOR(<<"hello">>, more_data, cause),
  ExInfo4 = 'clojerl.ExceptionInfo':?CONSTRUCTOR(<<"hello">>, data, some_cause),
  ExInfo5 = 'clojerl.ExceptionInfo':?CONSTRUCTOR(<<"Alo">>, data, cause),

  false   = clj_rt:equiv(ExInfo1, ExInfo3),
  false   = clj_rt:equiv(ExInfo1, ExInfo4),
  false   = clj_rt:equiv(ExInfo1, ExInfo5),

  ct:comment("An ExceptionInfo and something else"),
  false = clj_rt:equiv(ExInfo1, whatever),
  false = clj_rt:equiv(ExInfo1, #{}),

  {comments, ""}.

-spec fields(config()) -> result().
fields(_Config) ->
  ct:comment("Only ex-info with the same message, data and cause are equiv"),
  ExInfo1     = 'clojerl.ExceptionInfo':?CONSTRUCTOR(<<"hello">>, data),
  <<"hello">> = 'clojerl.ExceptionInfo':message(ExInfo1),
  data        = 'clojerl.ExceptionInfo':data(ExInfo1),
  ?NIL        = 'clojerl.ExceptionInfo':cause(ExInfo1),

  ExInfo2     = 'clojerl.ExceptionInfo':?CONSTRUCTOR(<<"hello">>, data, cause),
  <<"hello">> = 'clojerl.ExceptionInfo':message(ExInfo2),
  data        = 'clojerl.ExceptionInfo':data(ExInfo2),
  cause       = 'clojerl.ExceptionInfo':cause(ExInfo2),

  ok = try 'clojerl.ExceptionInfo':?CONSTRUCTOR(<<"hello">>, ?NIL), error
       catch error:Error ->
           Msg        = clj_rt:str(Error),
           Regex      = "Additional data must be non-nil",
           {match, _} = re:run(Msg, Regex),
           ok
       end,

  {comments, ""}.

-spec hash(config()) -> result().
hash(_Config) ->
  ExInfo1 = 'clojerl.ExceptionInfo':?CONSTRUCTOR(<<"hello">>, data, cause),
  Hash1 = 'clojerl.IHash':hash(ExInfo1),
  Hash1 = 'clojerl.IHash':hash(ExInfo1),

  ExInfo2 = 'clojerl.ExceptionInfo':?CONSTRUCTOR(<<"hello">>, more_data, cause),
  Hash2 = 'clojerl.IHash':hash(ExInfo2),

  true = Hash1 =/= Hash2,

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  ExInfo1 = 'clojerl.ExceptionInfo':?CONSTRUCTOR(<<"hello">>, data, cause),
  <<"clojerl.ExceptionInfo: hello :data">> = clj_rt:str(ExInfo1),

  {comments, ""}.
