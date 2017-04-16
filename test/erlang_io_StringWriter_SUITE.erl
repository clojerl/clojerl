-module(erlang_io_StringWriter_SUITE).

-include("clojerl.hrl").
-include("clj_test_utils.hrl").

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        ]).

-export([ str/1
        , write/1
        , close/1
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

-spec str(config()) -> result().
str(_Config) ->
  Writer = 'erlang.io.StringWriter':?CONSTRUCTOR(<<"hello">>),

  <<"hello">> = clj_rt:str(Writer),
  Writer = 'erlang.io.IWriter':write(Writer, <<" world!">>),
  <<"hello world!">> = clj_rt:str(Writer),

  ?NIL = 'erlang.io.Closeable':close(Writer),

  {comments, ""}.

-spec write(config()) -> result().
write(_Config) ->
  Writer = 'erlang.io.StringWriter':?CONSTRUCTOR(),

  Writer = 'erlang.io.IWriter':write(Writer, <<"hello">>),
  Writer = 'erlang.io.IWriter':write(Writer, <<" ">>),
  Writer = 'erlang.io.IWriter':write(Writer, <<"world!">>),

  <<"hello world!">> = clj_rt:str(Writer),

  Writer = 'erlang.io.IWriter':write(Writer, <<"~s!">>, [<<" Yeah">>]),

  <<"hello world! Yeah!">> = clj_rt:str(Writer),

  ?NIL = 'erlang.io.Closeable':close(Writer),

  {comments, ""}.

-spec close(config()) -> result().
close(_Config) ->
  Writer = 'erlang.io.StringWriter':?CONSTRUCTOR(<<"hello\nworld!\n">>),

  ?NIL = 'erlang.io.Closeable':close(Writer),

  ok = try ?NIL = 'erlang.io.Closeable':close(Writer), error
       catch _:_ -> ok
       end,

  {comments, ""}.

-spec complete_coverage(config()) -> result().
complete_coverage(_Config) ->
  Writer = 'erlang.io.StringWriter':?CONSTRUCTOR(<<"hello\nworld!\n">>),
  Pid    = Writer#?TYPE.data,

  ct:comment("Get the length"),
  13 = clj_rt:count(Writer),

  ct:comment("Send an unexpected message"),
  Pid ! {},

  ct:comment("Send an unexpected io_request"),
  {error, request} = io:request(Pid, unexpected),

  ct:comment("Send a put_chars io_request that fails"),
  Request = {put_chars, unicode, io, format, ["~s", []]},
  {error, format} = io:request(Pid, Request),

  ?NIL = 'erlang.io.Closeable':close(Writer),

  ok = try clj_rt:str(Writer), error
       catch _:_ -> ok
       end,

  ok = try clj_rt:count(Writer), error
       catch _:_ -> ok
       end,

  {comments, ""}.
