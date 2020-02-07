-module('clojerl.Keyword').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-behavior('clojerl.IFn').
-behavior('clojerl.IHash').
-behavior('clojerl.INamed').
-behavior('clojerl.IStringable').
-behavior('erlang.io.IWriter').
-behavior('erlang.io.IReader').

-export([ ?CONSTRUCTOR/1
        , ?CONSTRUCTOR/2
        , find/1
        , find/2
        ]).

-export([apply/2]).
-export([hash/1]).
-export([ read/1
        , read/2
        , read_line/1
        , skip/2
        ]).
-export([ write/2
        , write/3
        ]).
-export([ name/1
        , namespace/1
        ]).
-export([str/1]).

-type type() :: atom().

-spec ?CONSTRUCTOR('clojerl.Symbol':type() | binary()) -> type().
?CONSTRUCTOR(Name) when is_binary(Name) ->
  binary_to_atom(Name, utf8);
?CONSTRUCTOR(Name) when is_atom(Name) ->
  Name;
?CONSTRUCTOR(Symbol) ->
  binary_to_atom(clj_rt:str(Symbol), utf8).

-spec ?CONSTRUCTOR(binary(), binary()) -> type().
?CONSTRUCTOR(Namespace, Name)
  when is_binary(Namespace) andalso is_binary(Name) ->
  binary_to_atom(<<Namespace/binary, "/", Name/binary>>, utf8);
?CONSTRUCTOR(?NIL, Name) ->
  ?CONSTRUCTOR(Name).

-spec find(binary()) -> type().
find(Name) ->
  try
    binary_to_existing_atom(Name, utf8)
  catch
    _:_ -> ?NIL
  end.

-spec find(binary() | ?NIL, binary()) -> type().
find(?NIL, Name) ->
  find(Name);
find(Namespace, Name) ->
  try
    binary_to_existing_atom(<<Namespace/binary, "/", Name/binary>>, utf8)
  catch
    _:_ -> ?NIL
  end.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

%% clojerl.IFn

apply(Keyword, [Map]) ->
  clj_rt:get(Map, Keyword);
apply(Keyword, [Map, NotFound]) ->
  clj_rt:get(Map, Keyword, NotFound);
apply(_Keyword, Args) ->
  CountBin = integer_to_binary(length(Args)),
  ?ERROR(<<"Wrong number of args for keyword, got: ", CountBin/binary>>).

%% clojerl.IHash

hash(Keyword) ->
  erlang:phash2(Keyword).

%% clojerl.INamed

name(Keyword) ->
  KeywordBin = atom_to_binary(Keyword, utf8),
  case binary:split(KeywordBin, <<"/">>) of
    [_] -> KeywordBin;
    [_, Name] -> Name
  end.

namespace(Keyword) ->
  KeywordBin = atom_to_binary(Keyword, utf8),
  case binary:split(KeywordBin, <<"/">>) of
    [_] -> ?NIL;
    [Namespace, _] -> Namespace
  end.

%% clojerl.IStringable

str(Keyword) ->
  KeywordBin = atom_to_binary(Keyword, utf8),
  <<":", KeywordBin/binary>>.

%% erlang.io.IReader

read(IO) ->
  read(IO, 1).

read(IO, Length)
  when IO =:= standard_io; IO =:= standard_error ->
  io:get_chars(IO, "", Length);
read(Name, Length) ->
  case erlang:whereis(Name) of
    undefined ->
      error(<<"Invalid process name">>);
    _ ->
      io:get_chars(Name, "", Length)
  end.

read_line(IO)
  when IO =:= standard_io; IO =:= standard_error ->
  io:request(IO, {get_line, unicode, ""});
read_line(Name) ->
  case erlang:whereis(Name) of
    undefined ->
      error(<<"Invalid process name">>);
    _ ->
      io:request(Name, {get_line, unicode, ""})
  end.

skip(_IO, _Length) ->
  error(<<"unsupported operation: skip">>).

%% erlang.io.IWriter

write(Name, Str) when is_atom(Name), is_binary(Str) ->
  io:put_chars(Name, Str).

write(IO, Format, Values)
  when IO =:= standard_io; IO =:= standard_error ->
  ok = io:fwrite(IO, Format, clj_rt:to_list(Values)),
  IO;
write(Name, Str, Values) when is_atom(Name) ->
  case erlang:whereis(Name) of
    undefined ->
      error(<<"Invalid process name">>);
    _ ->
      io:fwrite(Name, Str, clj_rt:to_list(Values))
  end.
