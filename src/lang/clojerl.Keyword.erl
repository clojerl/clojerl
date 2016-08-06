-module('clojerl.Keyword').

-include("clojerl.hrl").

-behavior('clojerl.IFn').
-behavior('clojerl.IHash').
-behavior('clojerl.Named').
-behavior('clojerl.Stringable').
-behaviour('erlang.io.IWriter').
-behaviour('erlang.io.IReader').

-export([ ?CONSTRUCTOR/1
        , ?CONSTRUCTOR/2
        , find/1
        , find/2
        ]).

-export([invoke/2]).
-export([hash/1]).
-export([ read/1
        , read/2
        , read_line/1
        , skip/2
        , unread/2
        ]).
-export([ write/2
        , write/3
        ]).
-export([ name/1
        , namespace/1
        ]).
-export([str/1]).

-type type() :: atom().

-spec ?CONSTRUCTOR(binary()) -> type().
?CONSTRUCTOR(Name) when is_binary(Name) ->
  binary_to_atom(Name, utf8);
?CONSTRUCTOR(Name) when is_atom(Name) ->
  Name;
?CONSTRUCTOR(Symbol) ->
  binary_to_atom(clj_core:str(Symbol), utf8).

-spec ?CONSTRUCTOR(binary(), binary()) -> type().
?CONSTRUCTOR(Namespace, Name)
  when is_binary(Namespace) andalso is_binary(Name) ->
  binary_to_atom(<<Namespace/binary, "/", Name/binary>>, utf8);
?CONSTRUCTOR(undefined, Name) ->
  ?CONSTRUCTOR(Name).

-spec find(binary()) -> type().
find(Name) ->
  try
    binary_to_existing_atom(Name, utf8)
  catch
    _:_ -> undefined
  end.

-spec find(binary() | undefined, binary()) -> type().
find(undefined, Name) ->
  find(Name);
find(Namespace, Name) ->
  try
    binary_to_existing_atom(<<Namespace/binary, "/", Name/binary>>, utf8)
  catch
    _:_ -> undefined
  end.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

%% clojerl.IFn

invoke(Keyword, [Map]) ->
  clj_core:get(Map, Keyword);
invoke(Keyword, [Map, NotFound]) ->
  clj_core:get(Map, Keyword, NotFound);
invoke(_, Args) ->
  CountBin = integer_to_binary(length(Args)),
  throw(<<"Wrong number of args for keyword, got: ", CountBin/binary>>).

%% clojerl.IHash

hash(Keyword) when is_atom(Keyword) ->
  erlang:phash2(Keyword).

%% clojerl.Named

name(Keyword) ->
  KeywordBin = atom_to_binary(Keyword, utf8),
  case binary:split(KeywordBin, <<"/">>) of
    [_] -> KeywordBin;
    [_, Name] -> Name
  end.

namespace(Keyword) ->
  KeywordBin = atom_to_binary(Keyword, utf8),
  case binary:split(KeywordBin, <<"/">>) of
    [_] -> undefined;
    [Namespace, _] -> Namespace
  end.

%% clojerl.Stringable

str(Keyword) ->
  KeywordBin = atom_to_binary(Keyword, utf8),
  <<":", KeywordBin/binary>>.

%% erlang.io.IReader

read(IO) ->
  read(IO, 1).

read(IO, Length)
  when IO =:= standard_io; IO =:= standard_error ->
  maybe_binary(io:get_chars(IO, "", Length));
read(Name, Length) ->
  case erlang:whereis(Name) of
    undefined ->
      error(<<"Invalid process name">>);
    _ ->
      maybe_binary(io:get_chars(Name, "", Length))
  end.

read_line(IO)
  when IO =:= standard_io; IO =:= standard_error ->
  maybe_binary(io:request(IO, {get_line, unicode, ""}));
read_line(Name) ->
  case erlang:whereis(Name) of
    undefined ->
      error(<<"Invalid process name">>);
    _ ->
      maybe_binary(io:request(Name, {get_line, unicode, ""}))
  end.

skip(_IO, _Length) ->
  error(<<"unsupported operation: skip">>).

unread(_IO, _Ch) ->
  TypeName = atom_to_binary(?MODULE, utf8),
  error(<<"Unsupported operation: unread for ", TypeName/binary>>).

-spec maybe_binary(eof | string()) -> eof | binary().
maybe_binary(eof) -> eof;
maybe_binary(List) when is_list(List) -> list_to_binary(List).

%% erlang.io.IWriter

write(Name, Str) when is_atom(Name) ->
  write(Name, Str, []).

write(IO, Format, Values)
  when IO =:= standard_io; IO =:= standard_error ->
  ok = io:fwrite(IO, Format, clj_core:seq_to_list(Values)),
  IO;
write(Name, Str, Values) when is_atom(Name) ->
  case erlang:whereis(Name) of
    undefined ->
      error(<<"Invalid process name">>);
    _ ->
      io:fwrite(Name, Str, clj_core:seq_to_list(Values))
  end.
