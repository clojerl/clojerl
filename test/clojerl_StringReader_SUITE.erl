-module(clojerl_StringReader_SUITE).

-include("clojerl.hrl").

-export([all/0, init_per_suite/1]).

-export([ str/1
        , read/1
        , read_line/1
        , skip/1
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
  Reader = 'clojerl.StringReader':new(<<"hello">>),

  Str   = clj_core:str(Reader),
  Regex = <<"#<clojerl.StringReader \\d+\\.\\d+\\.\\d+>">>,
  match = re:run(Str, Regex, [{capture, none}]),

  undefined = 'clojerl.Closeable':close(Reader),

  {comments, ""}.

-spec read(config()) -> result().
read(_Config) ->
  Reader = 'clojerl.StringReader':new(<<"hello">>),

  <<"h">> = 'clojerl.IReader':read(Reader),
  <<"e">> = 'clojerl.IReader':read(Reader),
  <<"l">> = 'clojerl.IReader':read(Reader),
  <<"l">> = 'clojerl.IReader':read(Reader),
  <<"o">> = 'clojerl.IReader':read(Reader),
  eof     = 'clojerl.IReader':read(Reader),
  eof     = 'clojerl.IReader':read(Reader),

  undefined = 'clojerl.Closeable':close(Reader),

  Reader2 = 'clojerl.StringReader':new(<<"hello world!">>),

  <<"he">>      = 'clojerl.IReader':read(Reader2, 2),
  <<"llo">>     = 'clojerl.IReader':read(Reader2, 3),
  <<" ">>       = 'clojerl.IReader':read(Reader2),
  <<"world!">>  = 'clojerl.IReader':read(Reader2, 7),
  eof           = 'clojerl.IReader':read(Reader2),

  undefined = 'clojerl.Closeable':close(Reader2),

  {comments, ""}.

-spec read_line(config()) -> result().
read_line(_Config) ->
  Reader = 'clojerl.StringReader':new(<<"hello\nworld!\n">>),

  <<"hello">>  = 'clojerl.IReader':read_line(Reader),
  <<"world!">> = 'clojerl.IReader':read_line(Reader),
  eof          = 'clojerl.IReader':read_line(Reader),

  undefined = 'clojerl.Closeable':close(Reader),

  Reader2 = 'clojerl.StringReader':new(<<"hello\rworld!\rHello.">>),

  <<"hello">>  = 'clojerl.IReader':read_line(Reader2),
  <<"world!">> = 'clojerl.IReader':read_line(Reader2),
  <<"Hello.">> = 'clojerl.IReader':read_line(Reader2),
  eof          = 'clojerl.IReader':read_line(Reader2),

  undefined = 'clojerl.Closeable':close(Reader2),

  Reader3 = 'clojerl.StringReader':new(<<"hello\r\nworld!\r\n">>),

  <<"hello">>  = 'clojerl.IReader':read_line(Reader3),
  <<"world!">> = 'clojerl.IReader':read_line(Reader3),
  eof          = 'clojerl.IReader':read_line(Reader3),

  undefined = 'clojerl.Closeable':close(Reader3),

  {comments, ""}.

-spec skip(config()) -> result().
skip(_Config) ->
  Reader = 'clojerl.StringReader':new(<<"hello\nworld!\n">>),

  6            = 'clojerl.IReader':skip(Reader, 6),
  <<"world!">> = 'clojerl.IReader':read_line(Reader),
  eof          = 'clojerl.IReader':read_line(Reader),
  eof          = 'clojerl.IReader':skip(Reader, 1),

  undefined = 'clojerl.Closeable':close(Reader),

  {comments, ""}.

-spec close(config()) -> result().
close(_Config) ->
  Reader = 'clojerl.StringReader':new(<<"hello\nworld!\n">>),

  undefined = 'clojerl.Closeable':close(Reader),

  ok = try undefined = 'clojerl.Closeable':close(Reader), error
       catch _:_ -> ok
       end,

  {comments, ""}.

-spec complete_coverage(config()) -> result().
complete_coverage(_Config) ->
  Reader = 'clojerl.StringReader':new(<<"hello\nworld!\n">>),
  Pid    = Reader#?TYPE.data,

  ct:comment("Send an unexpected message"),
  Pid ! {},

  ct:comment("Send an unexpected io_request"),
  {error, request} = io:request(Pid, {io_request, self(), ref, unexpected}),

  undefined = 'clojerl.Closeable':close(Reader),

  {comments, ""}.
