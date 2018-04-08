-module('erlang.io.File').

-include("clojerl.hrl").

-behavior('erlang.io.Closeable').
-behavior('erlang.io.IReader').
-behavior('erlang.io.IWriter').
-behavior('clojerl.IStringable').

-export([open/1, open/2, path/1]).
-export([make_temp/2]).

-export([close/1]).
-export([  read/1
        ,  read/2
        ,  read_line/1
        ,  skip/2
        ]).
-export([ write/2
        , write/3
        ]).
-export([str/1]).

-type path() :: binary().
-type type() :: #{ ?TYPE => ?M
                 , pid   => pid() | file:fd()
                 , path  => path()
                 }.

-spec open(path()) -> type().
open(Path) when is_binary(Path) ->
  open(Path, [read]).

-spec open(path(), [atom()]) -> type().
open(Path, Modes) when is_binary(Path) ->
  case file:open(Path, Modes) of
    {ok, Pid}       -> #{?TYPE => ?M, pid => Pid, path => Path};
    {error, Reason} -> error(Reason)
  end.

-spec path(type()) -> binary().
path(#{?TYPE := ?M, path := Path}) ->
  Path.

-spec make_temp(binary(), binary()) -> type().
make_temp(Prefix, Suffix) ->
  TmpDir = tmp_dir(),
  ID     = erlang:integer_to_binary(erlang:phash2(erlang:make_ref())),
  Path   = <<TmpDir/binary, "/", Prefix/binary, ID/binary, Suffix/binary>>,
  open(Path, [write, read]).

-spec tmp_dir() -> binary().
tmp_dir() ->
  case os:type() of
    {win32, _} ->
      case os:getenv("TEMP") of
        false -> <<".">>;
        Tmp   -> erlang:list_to_binary(Tmp)
      end;
    {unix, _}  ->
      <<"/tmp">>
  end.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

close(#{?TYPE := ?M, pid := Pid, path := Path}) ->
  case file:close(Pid) of
    {error, _Reason} ->
      error(<<"Couldn't close ", Path/binary>>);
    ok ->
      ?NIL
  end.

read(File) ->
  read(File, 1).

read(#{?TYPE := ?M, pid := Pid}, Length) ->
  case io:get_chars(Pid, "", Length) of
    eof -> eof;
    Str -> list_to_binary(Str)
  end.

read_line(#{?TYPE := ?M, pid := Pid}) ->
  case io:request(Pid, {get_line, unicode, ""}) of
    eof -> eof;
    Str -> list_to_binary(Str)
  end.

skip(#{?TYPE := ?M}, _Length) ->
  TypeName = atom_to_binary(?MODULE, utf8),
  error(<<"Unsupported operation: skip for ", TypeName/binary>>).

write(#{?TYPE := ?M, pid := Pid} = SW, Str) ->
  ok = io:put_chars(Pid, Str),
  SW.

write(#{?TYPE := ?M, pid := Pid} = SW, Format, Values) ->
  ok = io:fwrite(Pid, Format, clj_rt:to_list(Values)),
  SW.

str(#{?TYPE := ?M, path := Path}) ->
  <<"#<erlang.io.File ", Path/binary, ">">>.
