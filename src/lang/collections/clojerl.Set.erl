-module('clojerl.Set').

-include("clojerl.hrl").

-behavior('clojerl.Counted').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IColl').
-behavior('clojerl.ILookup').
-behavior('clojerl.IMeta').
-behavior('clojerl.Seqable').
-behavior('clojerl.Stringable').

-export([new/1]).
-export(['clojerl.Counted.count'/1]).
-export([ 'clojerl.IColl.cons'/2
        , 'clojerl.IColl.empty'/1
        ]).
-export(['clojerl.IEquiv.equiv'/2]).
-export([ 'clojerl.IMeta.meta'/1
        , 'clojerl.IMeta.with_meta'/2
        ]).
-export([ 'clojerl.ILookup.get'/2
        , 'clojerl.ILookup.get'/3
        ]).
-export(['clojerl.Seqable.seq'/1]).
-export(['clojerl.Stringable.str'/1]).

-type type() :: #?TYPE{}.

-spec new(list()) -> type().
new(Values) when is_list(Values) ->
  #?TYPE{data = gb_sets:from_list(Values)}.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

%% clojerl.Counted

'clojerl.Counted.count'(#?TYPE{name = ?M, data = Set}) -> gb_sets:size(Set).

%% clojerl.Stringable

'clojerl.Stringable.str'(#?TYPE{name = ?M, data = Set}) ->
  Items = lists:map(fun clj_core:str/1, gb_sets:to_list(Set)),
  Strs = clj_utils:binary_join(Items, <<", ">>),
  <<"#{", Strs/binary, "}">>.

%% clojerl.Seqable

'clojerl.Seqable.seq'(#?TYPE{name = ?M, data = Set}) ->
  case gb_sets:size(Set) of
    0 -> undefined;
    _ -> gb_sets:to_list(Set)
  end.

%% clojerl.IMeta

'clojerl.IMeta.meta'(#?TYPE{name = ?M, info = Info}) ->
  maps:get(meta, Info, undefined).

'clojerl.IMeta.with_meta'(#?TYPE{name = ?M, info = Info} = Set, Metadata) ->
  Set#?TYPE{info = Info#{meta => Metadata}}.

%% clojerl.IColl

'clojerl.IColl.cons'(#?TYPE{name = ?M, data = Set}, X) ->
  Items = gb_sets:to_list(Set),
  clj_core:list([X | Items]).

'clojerl.IColl.empty'(_) -> new([]).

%% clojerl.IEquiv

'clojerl.IEquiv.equiv'( #?TYPE{name = ?M, data = X}
                      , #?TYPE{name = ?M, data = Y}
                      ) ->
  case gb_sets:size(X) == gb_sets:size(Y) of
    true ->
      X1 = gb_sets:to_list(X),
      Y1 = gb_sets:to_list(Y),
      clj_core:equiv(X1, Y1);
    false -> false
  end;
'clojerl.IEquiv.equiv'(_, _) -> false.

%% clojerl.ILookup

'clojerl.ILookup.get'(#?TYPE{name = ?M} = Set, Key) ->
  'clojerl.ILookup.get'(Set, Key, undefined).

'clojerl.ILookup.get'(#?TYPE{name = ?M, data = Set}, Key, NotFound) ->
  case gb_sets:is_member(Key, Set) of
    true -> Key;
    false -> NotFound
  end.
