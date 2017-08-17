-module('erlang.Type').

-include("clojerl.hrl").

-behavior('clojerl.IHash').
-behavior('clojerl.IStringable').

-export([ ?CONSTRUCTOR/1
        , module/1
        ]).
-export([hash/1]).
-export([str/1]).

-type type() :: #?TYPE{data :: module()}.

-spec ?CONSTRUCTOR(atom()) -> type().
?CONSTRUCTOR(Name) when is_atom(Name) ->
  #?TYPE{data = Name}.

-spec module(type()) -> module().
module(#?TYPE{name = ?M, data = Name}) -> Name.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

hash(#?TYPE{name = ?M, data = Name}) -> erlang:phash2(Name).

str(#?TYPE{name = ?M, data = Name}) -> erlang:atom_to_binary(Name, utf8).
