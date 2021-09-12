-module('clojerl.Symbol').

-include("clojerl.hrl").

-behavior('clojerl.IEquiv').
-behavior('clojerl.IFn').
-behavior('clojerl.IHash').
-behavior('clojerl.IMeta').
-behavior('clojerl.INamed').
-behavior('clojerl.IStringable').

-export([?CONSTRUCTOR/1, ?CONSTRUCTOR/2]).

-export([equiv/2]).
-export([apply/2]).
-export([hash/1]).
-export([ meta/1
        , with_meta/2
        ]).
-export([ name/1
        , namespace/1
        ]).
-export([str/1]).

-export_type([type/0]).
-type type() :: #{ ?TYPE => ?M
                 , ns    => ?NIL | binary()
                 , name  => binary()
                 , meta  => ?NIL | any()
                 }.

-spec ?CONSTRUCTOR(binary()) -> type().
?CONSTRUCTOR(<<"/">>)  ->
  ?CONSTRUCTOR(?NIL, <<"/">>);
?CONSTRUCTOR(Name0) when is_binary(Name0) ->
  case binary:split(Name0, <<"/">>) of
    [Namespace, Name1] ->
      ?CONSTRUCTOR(Namespace, Name1);
    _ ->
      ?CONSTRUCTOR(?NIL, Name0)
  end.

-spec ?CONSTRUCTOR(binary() | ?NIL, binary()) -> type().
?CONSTRUCTOR(Namespace, Name)
  when is_binary(Namespace) orelse Namespace == ?NIL,
       is_binary(Name) ->
  #{ ?TYPE => ?M
   , ns    => Namespace
   , name  => Name
   , meta  => ?NIL
   }.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

%% clojerl.IFn

apply(#{?TYPE := ?M} = Symbol, [Map]) ->
  clj_rt:get(Map, Symbol);
apply(#{?TYPE := ?M} = Symbol, [Map, NotFound]) ->
  clj_rt:get(Map, Symbol, NotFound);
apply(_, Args) ->
  CountBin = integer_to_binary(length(Args)),
  throw(<<"Wrong number of args for symbol, got: ", CountBin/binary>>).

%% clojerl.IStringable

str(#{?TYPE := ?M, ns := ?NIL, name := Name}) ->
  Name;
str(#{?TYPE := ?M, ns := Ns, name := Name}) ->
  <<Ns/binary, "/", Name/binary>>.

name(#{?TYPE := ?M, name := Name}) -> Name.

namespace(#{?TYPE := ?M, ns := Ns}) -> Ns.

meta(#{?TYPE := ?M, meta := Meta}) -> Meta.

with_meta(#{?TYPE := ?M} = Symbol, Metadata) ->
  Symbol#{meta => Metadata}.

equiv( #{?TYPE := ?M, ns := Ns, name := Name}
     , #{?TYPE := ?M, ns := Ns, name := Name}
     ) ->
  true;
equiv(_, _) ->
  false.

hash(#{?TYPE := ?M, ns := Ns, name := Name}) ->
  erlang:phash2({Ns, Name}).
