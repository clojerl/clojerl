-module(erlang_io_StringWriter_SUITE).

-include("clojerl.hrl").

-export([all/0, init_per_suite/1]).

-export([ str/1
        , write/1
        , close/1
        , complete_coverage/1
        ]).

-type config() :: list().
-type result() :: {comments, string()}.

-spec all() -> [atom()].
all() ->
  ExcludedFuns = [init_per_suite, end_per_suite, all, module_info],
  Exports = ?MODULE:module_info(exports),
  [F || {F, 1} <- Exports, not lists:member(F, ExcludedFuns)].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  application:ensure_all_started(clojerl),
  Config.

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------

-spec str(config()) -> result().
str(_Config) ->
  Writer = 'erlang.io.StringWriter':?CONSTRUCTOR(<<"hello">>),

  <<"hello">> = clj_core:str(Writer),
  Writer = 'erlang.io.IWriter':write(Writer, <<" world!">>),
  <<"hello world!">> = clj_core:str(Writer),

  undefined = 'erlang.io.Closeable':close(Writer),

  {comments, ""}.

-spec write(config()) -> result().
write(_Config) ->
  Writer = 'erlang.io.StringWriter':?CONSTRUCTOR(),

  Writer = 'erlang.io.IWriter':write(Writer, <<"hello">>),
  Writer = 'erlang.io.IWriter':write(Writer, <<" ">>),
  Writer = 'erlang.io.IWriter':write(Writer, <<"world!">>),

  <<"hello world!">> = clj_core:str(Writer),

  Writer = 'erlang.io.IWriter':write(Writer, <<"~s!">>, [<<" Yeah">>]),

  <<"hello world! Yeah!">> = clj_core:str(Writer),

  undefined = 'erlang.io.Closeable':close(Writer),

  {comments, ""}.

-spec close(config()) -> result().
close(_Config) ->
  Writer = 'erlang.io.StringWriter':?CONSTRUCTOR(<<"hello\nworld!\n">>),

  undefined = 'erlang.io.Closeable':close(Writer),

  ok = try undefined = 'erlang.io.Closeable':close(Writer), error
       catch _:_ -> ok
       end,

  {comments, ""}.

-spec complete_coverage(config()) -> result().
complete_coverage(_Config) ->
  Writer = 'erlang.io.StringWriter':?CONSTRUCTOR(<<"hello\nworld!\n">>),
  Pid    = Writer#?TYPE.data,

  ct:comment("Send an unexpected message"),
  Pid ! {},

  ct:comment("Send an unexpected io_request"),
  {error, request} = io:request(Pid, unexpected),

  ct:comment("Send a put_chars io_request that fails"),
  Request = {put_chars, unicode, io, format, ["~s", []]},
  {error, format} = io:request(Pid, Request),

  undefined = 'erlang.io.Closeable':close(Writer),

  ok = try clj_core:str(Writer), error
       catch _:_ -> ok
       end,

  {comments, ""}.
