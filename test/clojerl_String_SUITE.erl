-module(clojerl_String_SUITE).

-include("clojerl.hrl").
-include("clj_test_utils.hrl").

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        ]).

-export([ append/1
        , count/1
        , index_of/1
        , is_whitespace/1
        , last_index_of/1
        , join/1
        , to_lower/1
        , to_upper/1
        , seq/1
        , str/1
        , substring/1
        , split/1
        , replace/1
        , complete_coverage/1
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

-spec append(config()) -> result().
append(_Config) ->
  <<"ab">> = 'clojerl.String':append(<<"a">>, <<"b">>),
  <<"hello world">> = 'clojerl.String':append(<<"hello ">>, <<"world">>),

  {comments, ""}.

-spec count(config()) -> result().
count(_Config) ->
  3 = clj_rt:count(<<"abc">>),

  0 = clj_rt:count(<<>>),

  InvalidBinary = <<69, 82, 67, 80, 79, 0, 0, 0, 0, 0, 0, 0, 81, 0, 0, 0, 255,
                    255, 255, 255, 255, 255, 255, 255, 97, 0, 100, 0, 0, 0, 0,
                    0, 0, 0, 64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 125, 0, 11, 29, 97,
                    29, 98, 29, 99, 29, 100, 114, 0, 11, 0>>,
  ok = try clj_rt:count(InvalidBinary), error
       catch _:_ -> ok
       end,

  {comments, ""}.

-spec is_whitespace(config()) -> result().
is_whitespace(_Config) ->
  true = 'clojerl.String':is_whitespace(<<" ">>),
  true = 'clojerl.String':is_whitespace(<<"\t">>),
  true = 'clojerl.String':is_whitespace(<<"\n">>),
  true = 'clojerl.String':is_whitespace(<<"\r">>),

  false = 'clojerl.String':is_whitespace(<<"a">>),
  false = 'clojerl.String':is_whitespace(<<"A">>),
  false = 'clojerl.String':is_whitespace(<<"B">>),
  false = 'clojerl.String':is_whitespace(<<"b">>),

  {comments, ""}.

-spec index_of(config()) -> result().
index_of(_Config) ->
  1  = 'clojerl.String':index_of(<<"hello">>, <<"e">>),
  2  = 'clojerl.String':index_of(<<"hello">>, <<"l">>),
  -1 = 'clojerl.String':index_of(<<"hello">>, <<"f">>),
  -1 = 'clojerl.String':index_of(<<>>, <<"f">>),

  6  = 'clojerl.String':index_of(<<"foobarfoo">>, <<"f">>, 2),

  {comments, ""}.

-spec last_index_of(config()) -> result().
last_index_of(_Config) ->
  7  = 'clojerl.String':last_index_of(<<"hello world">>, <<"o">>),
  3  = 'clojerl.String':last_index_of(<<"hello">>, <<"l">>),
  -1 = 'clojerl.String':last_index_of(<<"hello">>, <<"f">>),
  -1 = 'clojerl.String':last_index_of(<<>>, <<"f">>),

  {comments, ""}.

-spec join(config()) -> result().
join(_Config) ->
  <<"1 and 2 and 3">> = 'clojerl.String':join([1, 2, 3], <<" and ">>),
  <<"42">> = 'clojerl.String':join([42], <<" and ">>),
  <<>> = 'clojerl.String':join([], <<" and ">>),

  {comments, ""}.

-spec to_lower(config()) -> result().
to_lower(_Config) ->
  <<"foo faa">> = 'clojerl.String':to_lower(<<"fOO FaA">>),
  <<"hello world!">> = 'clojerl.String':to_lower(<<"Hello WoRld!">>),

  {comments, ""}.

-spec to_upper(config()) -> result().
to_upper(_Config) ->
  <<"FOO FAA">> = 'clojerl.String':to_upper(<<"fOO FaA">>),
  <<"HELLO WORLD!">> = 'clojerl.String':to_upper(<<"Hello WoRld!">>),

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  <<"hello">> = clj_rt:str(<<"hello">>),
  <<>> = clj_rt:str(<<>>),

  {comments, ""}.

-spec seq(config()) -> result().
seq(_Config) ->
  5 = clj_rt:count(clj_rt:seq(<<"hello">>)),
  StringSeq = clj_rt:seq(<<"hello">>),
  StringSeq = 'clojerl.StringSeq':?CONSTRUCTOR(<<"hello">>),

  ?NIL = clj_rt:seq(<<>>),

  [<<"h">>, <<"e">>, <<"l">>, <<"l">>, <<"o">>] = clj_rt:to_list(<<"hello">>),

  {comments, ""}.

-spec substring(config()) -> result().
substring(_Config) ->
  <<"h">>   = 'clojerl.String':substring(<<"hello">>, 0, 1),
  <<"hel">> = 'clojerl.String':substring(<<"hello">>, 0, 3),
  <<"l">>   = 'clojerl.String':substring(<<"hello">>, 3, 4),
  <<"">>    = 'clojerl.String':substring(<<"hello">>, 4, 4),
  <<"o">>   = 'clojerl.String':substring(<<"hello">>, 4, 5),
  <<>>      = 'clojerl.String':substring(<<>>, 0, 1),
  <<"lo">>  = 'clojerl.String':substring(<<"hello">>, 3),

  ok = try 'clojerl.String':substring(<<>>, 5, 4), error
       catch _:_ -> ok
       end,

  {comments, ""}.

-spec split(config()) -> result().
split(_Config) ->
  [<<"h">>, <<"llo">>] = 'clojerl.String':split(<<"hello">>, <<"e">>),
  [<<"1">>, <<"2">>, <<"3">>] = 'clojerl.String':split(<<"1,2,3">>, <<",">>),

  {comments, ""}.

-spec replace(config()) -> result().
replace(_Config) ->
  <<"faabarfaa">> = 'clojerl.String':replace(<<"foobarfoo">>, <<"o">>, <<"a">>),
  <<"fbarf">> = 'clojerl.String':replace(<<"foobarfoo">>, <<"oo">>, <<"">>),

  {comments, ""}.

-spec complete_coverage(config()) -> result().
complete_coverage(_Config) ->
  true = 'clojerl.String':starts_with(<<>>, <<>>),

  true = 'clojerl.String':starts_with(<<"123456">>, <<"1">>),
  true = 'clojerl.String':starts_with(<<"123456">>, <<"12">>),
  true = 'clojerl.String':starts_with(<<"123456">>, <<"123">>),

  false = 'clojerl.String':starts_with(<<"123456">>, <<"a">>),

  true = 'clojerl.String':ends_with(<<>>, <<>>),
  true = 'clojerl.String':ends_with(<<"123456">>, <<"6">>),
  true = 'clojerl.String':ends_with(<<"123456">>, <<"56">>),
  true = 'clojerl.String':ends_with(<<"123456">>, <<"456">>),

  false = 'clojerl.String':ends_with(<<"123456">>, <<"789">>),
  false = 'clojerl.String':ends_with(<<"123456">>, <<"1234567">>),

  true = 'clojerl.String':contains(<<"123456">>, <<"234">>),
  true = 'clojerl.String':contains(<<"123456">>, <<"456">>),

  false = 'clojerl.String':contains(<<"123456">>, <<"354">>),
  false = 'clojerl.String':contains(<<"123456">>, <<"abc">>),

  <<"3">> = 'clojerl.String':char_at(<<"123456">>, 2),

  true  = erlang:is_integer('clojerl.IHash':hash(<<"123456">>)),

  true  = 'clojerl.String':is_printable(<<"123456">>),
  false = 'clojerl.String':is_printable(<<"123456", 0>>),

  {comments, ""}.
