-module('erlang.Type').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-behavior('clojerl.IHash').
-behavior('clojerl.IStringable').

-export([ ?CONSTRUCTOR/1
        , module/1
        , 'instance?'/2
        , 'satisfies?'/2
        , 'extends?'/2
        ]).

-export([hash/1]).
-export([str/1]).

-type type() :: #{ ?TYPE => ?M
                 , name  => atom()
                 }.

-spec ?CONSTRUCTOR(atom()) -> type().
?CONSTRUCTOR(Name) when is_atom(Name) ->
  #{?TYPE => ?M, name => Name}.

-spec module(type()) -> module().
module(#{?TYPE := ?M, name := Name}) -> Name.

-spec 'instance?'(type(), any()) -> boolean().
'instance?'(#{?TYPE := ?M, name := Name}, #{?TYPE := Name}) ->
  true;
'instance?'(#{?TYPE := ?M, name := Name}, X) ->
  clj_rt:type_module(X) == Name.

-spec 'satisfies?'(type(), any()) -> boolean().
'satisfies?'(#{?TYPE := ?M, name := Type}, #{?TYPE := ValueType}) ->
  Type:?SATISFIES(ValueType);
'satisfies?'(#{?TYPE := ?M, name := Type}, X) ->
  ValueType = clj_rt:type_module(X),
  Type:?SATISFIES(ValueType).

-spec 'extends?'(type(), type()) -> boolean().
'extends?'(#{?TYPE := ?M, name := Type}, #{?TYPE := ?M, name := ValueType}) ->
  Type:?SATISFIES(ValueType).

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

hash(#{?TYPE := ?M, name := Name}) -> erlang:phash2(Name).

str(#{?TYPE := ?M, name := Name}) -> erlang:atom_to_binary(Name, utf8).
