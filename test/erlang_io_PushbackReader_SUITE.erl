-module(erlang_io_PushbackReader_SUITE).

-include("clojerl.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).

-export([ str/1
        , read/1
        , read_line/1
        , skip/1
        , unread/1
        , at_line_start/1
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
  file:write_file(<<"tmp">>, <<"Hello world!\n">>),
  Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  file:delete(<<"tmp">>),
  Config.

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------

-spec str(config()) -> result().
str(_Config) ->
  File   = 'erlang.io.File':open(<<"tmp">>),
  Reader = 'erlang.io.PushbackReader':new(File),

  Str   = clj_core:str(Reader),
  Regex = <<"#<erlang.io.PushbackReader \\d+\\.\\d+\\.\\d+>">>,
  match = re:run(Str, Regex, [{capture, none}]),

  undefined = 'erlang.io.Closeable':close(Reader),

  {comments, ""}.

-spec read(config()) -> result().
read(_Config) ->
  File   = 'erlang.io.File':open(<<"tmp">>),
  Reader = 'erlang.io.PushbackReader':new(File),

  <<"Hello">> = 'erlang.io.IReader':read(Reader, 5),
  Reader      = 'erlang.io.IReader':unread(Reader, <<"Hello">>),
  <<"H">>     = 'erlang.io.IReader':read(Reader),
  <<"ello ">> = 'erlang.io.IReader':read(Reader, 5),
  <<"world">> = 'erlang.io.IReader':read(Reader, 5),
  <<"!\n">>   = 'erlang.io.IReader':read(Reader, 2),
  Reader      = 'erlang.io.IReader':unread(Reader, <<"yeah">>),
  <<"yeah">>  = 'erlang.io.IReader':read(Reader, 10),
  eof         = 'erlang.io.IReader':read(Reader),

  undefined = 'erlang.io.Closeable':close(Reader),

  {comments, ""}.

-spec read_line(config()) -> result().
read_line(_Config) ->
  File   = 'erlang.io.File':open(<<"tmp">>),
  Reader = 'erlang.io.PushbackReader':new(File),

  <<"Hello world!\n">> = 'erlang.io.IReader':read_line(Reader),
  Reader = 'erlang.io.IReader':unread(Reader, <<"hello\r">>),
  <<"hello">> = 'erlang.io.IReader':read_line(Reader),
  Reader = 'erlang.io.IReader':unread(Reader, <<"hello\r\n">>),
  <<"hello">> = 'erlang.io.IReader':read_line(Reader),
  Reader = 'erlang.io.IReader':unread(Reader, <<"hello\n">>),
  <<"hello">> = 'erlang.io.IReader':read_line(Reader),

  eof = 'erlang.io.IReader':read_line(Reader),

  undefined = 'erlang.io.Closeable':close(Reader),

  {comments, ""}.

-spec skip(config()) -> result().
skip(_Config) ->
  StrReader = 'erlang.io.StringReader':new(<<"Hello world!\n">>),
  Reader    = 'erlang.io.PushbackReader':new(StrReader),

  6            = 'erlang.io.IReader':skip(Reader, 6), %% Skipped "Hello "
  <<"world!">> = 'erlang.io.IReader':read_line(Reader),
  Reader       = 'erlang.io.IReader':unread(Reader, <<"world!">>),
  3            = 'erlang.io.IReader':skip(Reader, 3), %% Skipped "wor"
  <<"ld">>     = 'erlang.io.IReader':read(Reader, 2),
  eof          = 'erlang.io.IReader':skip(Reader, 3), %% Skipped "!\n"
  eof          = 'erlang.io.IReader':read_line(Reader),
  eof          = 'erlang.io.IReader':skip(Reader, 1),

  undefined = 'erlang.io.Closeable':close(Reader),

  {comments, ""}.

-spec unread(config()) -> result().
unread(_Config) ->
  File   = 'erlang.io.File':open(<<"tmp">>),
  Reader = 'erlang.io.PushbackReader':new(File),

  <<"Hello">> = 'erlang.io.IReader':read(Reader, 5),
  <<" ">>     = 'erlang.io.IReader':read(Reader),
  Reader      = 'erlang.io.IReader':unread(Reader, <<"_ _">>),
  <<"_ _">>   = 'erlang.io.IReader':read(Reader, 3),
  <<"world">> = 'erlang.io.IReader':read(Reader, 5),
  <<"!\n">>   = 'erlang.io.IReader':read(Reader, 2),
  eof         = 'erlang.io.IReader':read(Reader),

  ok = try 'erlang.io.IReader':unread(Reader, something), error
       catch _:_ -> ok
       end,

  undefined = 'erlang.io.Closeable':close(Reader),

  {comments, ""}.

-spec at_line_start(config()) -> result().
at_line_start(_Config) ->
  File   = 'erlang.io.File':open(<<"tmp">>),
  Reader = 'erlang.io.PushbackReader':new(File),

  true         = 'erlang.io.PushbackReader':at_line_start(Reader),
  <<"Hello ">> = 'erlang.io.IReader':read(Reader, 6),
  false        = 'erlang.io.PushbackReader':at_line_start(Reader),
  <<"world">>  = 'erlang.io.IReader':read(Reader, 5),
  <<"!\n">>    = 'erlang.io.IReader':read(Reader, 2),
  true         = 'erlang.io.PushbackReader':at_line_start(Reader),
  eof          = 'erlang.io.IReader':read(Reader),

  undefined = 'erlang.io.Closeable':close(Reader),

  ct:comment("At line start fails"),
  FakePid = spawn_link(fun fake_loop/0),
  FakeReader = Reader#?TYPE{data = FakePid},
  ok = try 'erlang.io.PushbackReader':at_line_start(FakeReader), error
       catch _:_ -> ok
       end,

  {comments, ""}.

-spec close(config()) -> result().
close(_Config) ->
  File   = 'erlang.io.File':open(<<"tmp">>),
  Reader = 'erlang.io.PushbackReader':new(File),

  undefined = 'erlang.io.Closeable':close(Reader),

  ok = try undefined = 'erlang.io.Closeable':close(Reader), error
       catch _:_ -> ok
       end,

  {comments, ""}.

-spec complete_coverage(config()) -> result().
complete_coverage(_Config) ->
  File   = 'erlang.io.File':open(<<"tmp">>),
  Reader = 'erlang.io.PushbackReader':new(File),
  Pid    = Reader#?TYPE.data,

  ct:comment("Send an unexpected message"),
  Pid ! {},

  ct:comment("Send an unexpected io_request"),
  {error, request} = io:request(Pid, {io_request, self(), ref, unexpected}),

  undefined = 'erlang.io.Closeable':close(Reader),

  ct:comment("Generate error when closing a PushbackReader that wraps a file"),
  File1   = 'erlang.io.File':open(<<"tmp">>),
  Reader1 = 'erlang.io.PushbackReader':new(File1),
  meck:new(file, [passthrough, unstick]),
  try
    meck:expect(file, close, fun(_) -> {error, fake} end),
    ok = try 'erlang.io.Closeable':close(Reader1), error
         catch _:_ -> ok
         end
  after
    meck:unload(file)
  end,

  {comments, ""}.

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

-spec fake_loop() -> ok.
fake_loop() ->
  receive
    {From, Ref, at_line_start} ->
      From ! {Ref, {error, failed}},
      fake_loop()
  end.
