-module('erlang.Type').

-include("clojerl.hrl").

-behavior('clojerl.IHash').
-behavior('clojerl.IStringable').

-export([ ?CONSTRUCTOR/1
        , module/1
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

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

hash(#{?TYPE := ?M, name := Name}) -> erlang:phash2(Name).

str(#{?TYPE := ?M, name := Name}) -> erlang:atom_to_binary(Name, utf8).
