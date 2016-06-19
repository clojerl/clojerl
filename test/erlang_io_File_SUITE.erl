-module(erlang_io_File_SUITE).

-include("clojerl.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).

-export([ str/1
        , read/1
        , read_line/1
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
  file:write_file(<<"tmp">>, <<"Hello world!\nHow are you?\r\n">>),
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
  File = 'erlang.io.File':open(<<"tmp">>, [read]),

  Str   = clj_core:str(File),
  Regex = <<"#<erlang.io.File \\d+\\.\\d+\\.\\d+>">>,
  match = re:run(Str, Regex, [{capture, none}]),

  undefined = 'clojerl.Closeable':close(File),

  {comments, ""}.

-spec read(config()) -> result().
read(_Config) ->
  File = 'erlang.io.File':open(<<"tmp">>, [read]),

  <<"H">>      = 'clojerl.IReader':read(File),
  <<"e">>      = 'clojerl.IReader':read(File),
  <<"l">>      = 'clojerl.IReader':read(File),
  <<"l">>      = 'clojerl.IReader':read(File),
  <<"o">>      = 'clojerl.IReader':read(File),
  <<" world">> = 'clojerl.IReader':read(File, 6),
  <<"!\n">>    = 'clojerl.IReader':read(File, 2),
  <<"How are you?\r\n">> = 'clojerl.IReader':read(File, 14),
  eof     = 'clojerl.IReader':read(File),

  undefined = 'clojerl.Closeable':close(File),

  {comments, ""}.

-spec read_line(config()) -> result().
read_line(_Config) ->
  File = 'erlang.io.File':open(<<"tmp">>, [read]),

  <<"Hello world!\n">>   = 'clojerl.IReader':read_line(File),
  <<"How are you?\n">> = 'clojerl.IReader':read_line(File),
  eof                    = 'clojerl.IReader':read_line(File),

  undefined = 'clojerl.Closeable':close(File),

  {comments, ""}.

-spec write(config()) -> result().
write(_Config) ->
  Filename = <<"tmp-write">>,

  File = 'erlang.io.File':open(Filename, [append]),

  File = 'clojerl.IWriter':write(File, <<"hello">>),
  File = 'clojerl.IWriter':write(File, <<" ">>),
  File = 'clojerl.IWriter':write(File, <<"world!">>),

  File = 'clojerl.IWriter':write(File, <<"~s!">>, [<<" Yeah">>]),

  undefined = 'clojerl.Closeable':close(File),

  {ok, <<"hello world! Yeah!">>} = file:read_file(<<"tmp-write">>),

  file:delete(Filename),

  {comments, ""}.

-spec close(config()) -> result().
close(_Config) ->
  ct:comment("Open an existing file and close it"),
  File = 'erlang.io.File':open(<<"tmp">>),
  undefined = 'clojerl.Closeable':close(File),

  ct:comment("Closing it again shouldn't be a problem"),
  undefined = 'clojerl.Closeable':close(File),

  {comments, ""}.

-spec complete_coverage(config()) -> result().
complete_coverage(_Config) ->
  File = 'erlang.io.File':open(<<"tmp">>),

  ct:comment("Skip is unsupported"),
  ok = try 'clojerl.IReader':skip(File, 10), error
       catch _:_ -> ok
       end,

  ct:comment("Unread is unsupported"),
  ok = try 'clojerl.IReader':unread(File, <<>>), error
       catch _:_ -> ok
       end,

  ct:comment("Open non-existing file"),
  ok = try 'erlang.io.File':open(<<"bla">>), error
       catch _:_ -> ok
       end,

  meck:new(file, [passthrough, unstick]),
  try
    meck:expect(file, close, fun(_) -> {error, fake} end),
    ok = try 'clojerl.Closeable':close(File), error
         catch _:_ -> ok
         end
  after
    meck:unload(file)
  end,

  {comments, ""}.
