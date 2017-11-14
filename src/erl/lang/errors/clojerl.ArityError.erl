-module('clojerl.ArityError').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-behavior('clojerl.IEquiv').
-behavior('clojerl.IError').
-behavior('clojerl.IHash').
-behavior('clojerl.IStringable').

-export([?CONSTRUCTOR/2]).

-export([equiv/2]).
-export([message/1]).
-export([hash/1]).
-export([str/1]).

-type type() :: #{ ?TYPE  => ?M
                 , actual => arity()
                 , name   => binary()
                 }.

-spec ?CONSTRUCTOR(arity(), binary()) -> type().
?CONSTRUCTOR(Arity, Name) when is_integer(Arity), is_binary(Name) ->
  #{ ?TYPE  => ?M
   , actual => Arity
   , name   => Name
   }.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

%% clojerl.IEquiv

equiv(#{?TYPE := ?M} = X, #{?TYPE := ?M} = X) ->
  true;
equiv(_, _) ->
  false.

%% clojerl.IError

message(#{?TYPE := ?M, actual := Actual, name := Name}) ->
  ActualBin = integer_to_binary(Actual),
  <<"Wrong number of args (", ActualBin/binary, ") passed to: ", Name/binary>>.

%% clojerl.IHash

hash(#{?TYPE := ?M} = Error) ->
  erlang:phash2(Error).

%% clojerl.IStringable

str(#{?TYPE := ?M} = Error) ->
  clj_utils:error_str(?M, message(Error)).
