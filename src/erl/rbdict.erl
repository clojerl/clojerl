%% Copyright (c) 2008 Robert Virding. All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%%
%% 1. Redistributions of source code must retain the above copyright
%%    notice, this list of conditions and the following disclaimer.
%% 2. Redistributions in binary form must reproduce the above copyright
%%    notice, this list of conditions and the following disclaimer in the
%%    documentation and/or other materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%% FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%% COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%% ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.

-module(rbdict).

-include("clojerl.hrl").

%% Standard interface.
-export([ new/0
        , new/1
        , compare_fun/1
        , is_key/2
        , to_list/1
        , from_list/1
        , from_list/2
        , size/1
        ]).
-export([ fetch/2
        , find/2
        , fetch_keys/1
        , erase/2
        ]).
-export([ store/3
        , append/3
        , append_list/3
        ]).
-export([ update_val/3
        , update/3
        , update/4
        , update_counter/3
        ]).
-export([ fold/3
        , map/2
        , filter/2
        , merge/3
        ]).

-type n() :: {empty, fun()} | {r | b, n(), any(), any(), n(), fun()}.

%% The algorithms here are taken directly from Okasaki and Rbset in
%% ML/Scheme. The interface is compatible with the standard dict
%% interface.
%%
%% The following structures are used to build the the RB-dict:
%%
%% {r, Left, Key, Val, Right}
%% {b, Left, Key, Val, Right}
%% empty
%%
%% It is interesting to note that expanding out the first argument of
%% l/rbalance, the colour, in store etc. is actually slower than not
%% doing it. Measured.

%% new() -> Dict.

new() -> new(fun default_compare/2).

new(Compare) when is_function(Compare) ->
  F = fun(X, Y) ->
          case Compare(X, Y) of
            true  -> 1;
            false -> -1;
            Z     -> Z
          end
      end,
  {empty, F}.

compare_fun({empty, Compare}) -> Compare;
compare_fun({_, _, _, _, _, Compare}) -> Compare.

default_compare(X, X)          -> 0;
default_compare(?NIL, _)       -> -1;
default_compare(_, ?NIL)       -> 1;
default_compare(X, Y) when X < Y -> -1;
default_compare(X, Y) when X > Y -> 1.

%% is_key(Key, Dict) -> true | false.

is_key(_, {empty, _}) -> false;
is_key(K, {_, Left, K1, _, Right, Compare}) when K < K1 ->
  case Compare(K, K1) of
    -1 -> is_key(K, Left);
    1  -> is_key(K, Right);
    0  -> true
  end.

%% to_list(Dict) -> [{Key,Value}].

to_list(T) -> to_list(T, []).

to_list({empty, _}, List) -> List;
to_list({_, A, Xk, Xv, B, _}, List) ->
  to_list(A, [{Xk, Xv}|to_list(B, List)]).

%% from_list([{Key, Value}]) -> Dict.

from_list(L) ->
  from_list(fun default_compare/2, L).

from_list(Compare, L) ->
  lists:foldl(fun ({K, V}, D) -> store(K, V, D) end, new(Compare), L).

%% size(Dict) -> int().

size(T) -> size1(T).

size1({empty, _}) -> 0;
size1({_, L, _, _, R, _}) ->
  size1(L) + size1(R) + 1.

%% fetch(Key, Dict) -> Value.

fetch(K, {_, Left, K1, Val, Right, Compare}) ->
  case Compare(K, K1) of
    -1 -> fetch(K, Left);
    1  -> fetch(K, Right);
    0  -> Val
  end.

%% find(Key, Dict) -> {ok, Value} | error.

find(_, {empty, _}) -> error;
find(K, {_, Left, K1, Val, Right, Compare}) ->
  case Compare(K, K1) of
    -1 -> find(K, Left);
    1  -> find(K, Right);
    0  -> {ok, Val}
  end.

%% fetch_keys(Dict) -> [Key].

fetch_keys(T) -> fetch_keys(T, []).

fetch_keys({empty, _}, Tail) -> Tail;
fetch_keys({_, L, K, _, R, _}, Tail) ->
  fetch_keys(L, [K | fetch_keys(R, Tail)]).

%% store(Key, Val, Dict) -> Dict.

store(K, V, T) ->
  {_, L, K1, V1, R, Compare} = store1(K, V, T),
  {b, L, K1, V1, R, Compare}.                %setelement(1, b, T1).

store1(K, V, {empty, Compare}) ->
  {r, {empty, Compare}, K, V, {empty, Compare}, Compare};
store1(K, V, {C, Left, K1, V1, Right, Compare}) ->
  case Compare(K, K1) of
    -1 -> lbalance(C, store1(K, V, Left), K1, V1, Right, Compare);
    1  -> rbalance(C, Left, K1, V1, store1(K, V, Right), Compare);
    0  -> {C, Left, K, V, Right, Compare}
  end.

%% Expanding out l/rbalance is slower!
%% store1(K, V, empty) -> {r, empty, K, V, empty};
%% store1(K, V, {r, Left, K1, V1, Right}) ->
%%     if K < K1 -> {r, store1(K, V, Left), K1, V1, Right};
%%        K > K1 -> {r, Left, K1, V1, store1(K, V, Right)};
%%        true -> {r, Left, K, V, Right}
%%     end;
%% store1(K, V, {b, Left, K1, V1, Right}) ->
%%     if K < K1 ->
%%         lbalance(store1(K, V, Left), K1, V1, Right);
%%        K > K1 ->
%%         rbalance(Left, K1, V1, store1(K, V, Right));
%%        true -> {b, Left, K, V, Right}
%%     end.

%% append(Key, Val, Dict) -> Dict.

append(K, V, T) ->
  {_, L, K1, V1, R, Compare} = append1(K, V, T),
  {b, L, K1, V1, R, Compare}.                %setelement(1, b, T1).

append1(K, V, {empty, Compare}) ->
  {r, {empty, Compare}, K, [V], {empty, Compare}};
append1(K, V, {C, Left, K1, V1, Right, Compare}) ->
  case Compare(K, K1) of
    -1 -> lbalance(C, append1(K, V, Left), K1, V1, Right, Compare);
    1  -> rbalance(C, Left, K1, V1, append1(K, V, Right), Compare);
    0  -> {C, Left, K, V1 ++ [V], Right, Compare}
  end.

%% append(Key, [Val], Dict) -> Dict.

append_list(K, V, T) ->
  {_, L, K1, V1, R, Compare} = append_list1(K, V, T),
  {b, L, K1, V1, R, Compare}.                %setelement(1, b, T1).

append_list1(K, V, {empty, Compare}) ->
  {r, {empty, Compare}, K, V, {empty, Compare}};
append_list1(K, V, {C, Left, K1, V1, Right, Compare}) ->
  case Compare(K, K1) of
    -1 -> lbalance(C, append_list1(K, V, Left), K1, V1, Right, Compare);
    1  -> rbalance(C, Left, K1, V1, append_list1(K, V, Right), Compare);
    0  -> {C, Left, K, V1 ++ V, Right, Compare}
  end.

%% update_val(Key, Val, Dict) -> Dict.

update_val(K, V, {RB, A, Xk, Xv, B, Compare}) ->
  case Compare(K, Xk) of
    -1 -> {RB, update_val(K, V, A), Xk, Xv, B, Compare};
    1  -> {RB, A, Xk, Xv, update_val(K, V, B), Compare};
    0  -> {RB, A, Xk, V, B, Compare}
  end.

%% update(Key, Fun, Dict) -> Dict.

update(K, F, {RB, A, Xk, Xv, B, Compare}) ->
  case Compare(K, Xk) of
    -1 -> {RB, update(K, F, A), Xk, Xv, B, Compare};
    1  -> {RB, A, Xk, Xv, update(K, F, B), Compare};
    0  -> {RB, A, Xk, F(Xv), B, Compare}
  end.

%% update(Key, Fun, Init, Dict) -> Dict.

update(K, F, I, T) ->
  {_, L, K1, V1, R, Compare} = update1(K, F, I, T),
  {b, L, K1, V1, R, Compare}.                %setelement(1, b, T1).

update1(K, _, I, {empty, Compare}) ->
  {r, {empty, Compare}, K, I, {empty, Compare}, Compare};
update1(K, F, I, {RB, A, Xk, Xv, B, Compare}) ->
  case Compare(K, Xk) of
    -1 -> lbalance(RB, update1(K, F, I, A), Xk, Xv, B, Compare);
    1  -> rbalance(RB, A, Xk, Xv, update1(K, F, I, B), Compare);
    0  -> {RB, A, Xk, F(Xv), B, Compare}
  end.

%% update_counter(Key, Incr, Dict) -> Dict.

update_counter(K, I, T) ->
  {_, L, K1, V1, R, Compare} = update_counter1(K, I, T),
  {b, L, K1, V1, R, Compare}.                %setelement(1, b, T1).

update_counter1(K, I, {empty, Compare}) ->
  {r, {empty, Compare}, K, I, {empty, Compare}, Compare};
update_counter1(K, I, {RB, A, Xk, Xv, B, Compare}) ->
  case Compare(K, Xk) of
    -1 -> lbalance(RB, update_counter1(K, I, A), Xk, Xv, B, Compare);
    1  -> rbalance(RB, A, Xk, Xv, update_counter1(K, I, B), Compare);
    0  -> {RB, A, Xk, Xv + I, B}
  end.

%% lbalance(Colour, Left, Key, Val, Right).
%% rbalance(Colour, Left, Key, Val, Right).
%% Balance a tree afer (possibly) adding a node to the left/right.

lbalance(b, {r, {r, A, Xk, Xv, B, F}, Yk, Yv, C, F}, Zk, Zv, D, F) ->
  {r, {b, A, Xk, Xv, B, F}, Yk, Yv, {b, C, Zk, Zv, D, F}, F};
lbalance(b, {r, A, Xk, Xv, {r, B, Yk, Yv, C, F}, F}, Zk, Zv, D, F) ->
  {r, {b, A, Xk, Xv, B, F}, Yk, Yv, {b, C, Zk, Zv, D, F}, F};
lbalance(C, A, Xk, Xv, B, F) -> {C, A, Xk, Xv, B, F}.

rbalance(b, A, Xk, Xv, {r, {r, B, Yk, Yv, C, F}, Zk, Zv, D, F}, F) ->
  {r, {b, A, Xk, Xv, B, F}, Yk, Yv, {b, C, Zk, Zv, D, F}, F};
rbalance(b, A, Xk, Xv, {r, B, Yk, Yv, {r, C, Zk, Zv, D, F}, F}, F) ->
  {r, {b, A, Xk, Xv, B, F}, Yk, Yv, {b, C, Zk, Zv, D, F}, F};
rbalance(C, A, Xk, Xv, B, F) -> {C, A, Xk, Xv, B, F}.

%% erase(Key, Dict) -> Dict.

erase(K, T) ->
  {T1, _} = erase_aux(K, T),
  T1.

%% erase_aux(Key, Node) -> {Node, Decreased}.

erase_aux(_, {empty, F}) -> {{empty, F}, false};
erase_aux(K, {b, A, Xk, Xv, B, F}) ->
  case F(K, Xk) of
    -1 ->
      {A1, Dec} = erase_aux(K, A),
      if  Dec -> unbalright(b, A1, Xk, Xv, B, F);
          true -> {{b, A1, Xk, Xv, B, F}, false}
      end;
    1 ->
      {B1, Dec} = erase_aux(K, B),
      if  Dec -> unballeft(b, A, Xk, Xv, B1, F);
          true -> {{b, A, Xk, Xv, B1, F}, false}
      end;
    0 ->
      case B of
        {empty, _} -> blackify(A);
        _ ->
          {B1, {Mk, Mv}, Dec} = erase_min(B),
          erase_aux2(Dec, b, A, Mk, Mv, B1, F)
      end
  end;
erase_aux(K, {r, A, Xk, Xv, B, F}) ->
  case F(K, Xk) of
    -1 ->
      {A1, Dec} = erase_aux(K, A),
      if  Dec -> unbalright(r, A1, Xk, Xv, B, F);
          true -> {{r, A1, Xk, Xv, B, F}, false}
      end;
    1 ->
      {B1, Dec} = erase_aux(K, B),
      if  Dec -> unballeft(r, A, Xk, Xv, B1, F);
          true -> {{r, A, Xk, Xv, B1, F}, false}
      end;
    0 ->
      case B of
        {empty, _} -> {A, false};
        _ ->
          {B1, {Mk, Mv}, Dec} = erase_min(B),
          erase_aux2(Dec, r, A, Mk, Mv, B1, F)
      end
  end.

erase_aux2(true = _Dec, Color, A, Mk, Mv, B1, F) ->
  unballeft(Color, A, Mk, Mv, B1, F);
erase_aux2(_, Color, A, Mk, Mv, B1, F) ->
  {{Color, A, Mk, Mv, B1, F}, false}.

%% erase_min(Node) -> {Node, {NodeKey, NodeVal}, Decreased}.

erase_min({b, {empty, F}, Xk, Xv, {empty, F}, F}) ->
  {{empty, F}, {Xk, Xv}, true};
erase_min({b, {empty, F}, Xk, Xv, {r, A, Yk, Yv, B, F}, F}) ->
  {{b, A, Yk, Yv, B, F}, {Xk, Xv}, false};
erase_min({b, {empty, _}, _, _, {b, _, _, _, _, _}, _}) -> exit(boom);
erase_min({r, {empty, F}, Xk, Xv, A, F}) ->
  {A, {Xk, Xv}, false};
%% Rec from left
erase_min({b, A, Xk, Xv, B, F}) ->
  {A1, Min, Dec} = erase_min(A),
  if Dec ->
      {T, Dec1} = unbalright(b, A1, Xk, Xv, B, F),
      {T, Min, Dec1};
     true -> {{b, A1, Xk, Xv, B, F}, Min, false}
  end;
erase_min({r, A, Xk, Xv, B, F}) ->
  {A1, Min, Dec} = erase_min(A),
  if Dec ->
      {T, Dec1} = unbalright(r, A1, Xk, Xv, B, F),
      {T, Min, Dec1};
     true -> {{r, A1, Xk, Xv, B, F}, Min, false}
  end.

blackify({r, A, K, V, B, F}) -> {{b, A, K, V, B, F}, false};
blackify(Node) -> {Node, true}.

unballeft(r, {b, A, Xk, Xv, B, F}, Yk, Yv, C, F) ->
  {lbalance(b, {r, A, Xk, Xv, B, F}, Yk, Yv, C, F), false};
unballeft(b, {b, A, Xk, Xv, B, F}, Yk, Yv, C, F) ->
  {lbalance(b, {r, A, Xk, Xv, B, F}, Yk, Yv, C, F), true};
unballeft(b, {r, A, Xk, Xv, {b, B, Yk, Yv, C, F}, F}, Zk, Zv, D, F) ->
  {{b, A, Xk, Xv, lbalance(b, {r, B, Yk, Yv, C, F}, Zk, Zv, D, F), F}, false}.

unbalright(r, A, Xk, Xv, {b, B, Yk, Yv, C, F}, F) ->
  {rbalance(b, A, Xk, Xv, {r, B, Yk, Yv, C, F}, F), false};
unbalright(b, A, Xk, Xv, {b, B, Yk, Yv, C, F}, F) ->
  {rbalance(b, A, Xk, Xv, {r, B, Yk, Yv, C, F}, F), true};
unbalright(b, A, Xk, Xv, {r, {b, B, Yk, Yv, C, F}, Zk, Zv, D, F}, F) ->
  {{b, rbalance(b, A, Xk, Xv, {r, B, Yk, Yv, C, F}, F), Zk, Zv, D, F}, false}.

%% fold(Fun, Acc, Dict) -> Acc.

fold(_, Acc, {empty, _}) -> Acc;
fold(F, Acc, {_, A, Xk, Xv, B, _}) ->
  fold(F, F(Xk, Xv, fold(F, Acc, B)), A).

%% map(Fun, Dict) -> Dict.

map(_, {empty, _} = T) -> T;
map(F, {RB, A, Xk, Xv, B, Compare}) ->
  {RB, map(F, A), Xk, F(Xk, Xv), map(F, B), Compare}.

%% filter(Fun, Dict) -> Dict.

filter(F, T) -> filter(F, T, new()).

filter(_, {empty, _}, New) -> New;
filter(F, {_, A, Xk, Xv, B, _}, New0) ->
  New1 = filter(F, A, New0),
  New2 = case F(Xk, Xv) of
           true -> store(Xk, Xv, New1);
           false -> New1
         end,
  filter(F, B, New2).

%% merge(Fun, Dict, Dict) -> Dict.

merge(F, D1, D2) ->
  fold(fun (K, V2, D) ->
           update(K, fun(V1) -> F(K, V1, V2) end, V2, D)
       end, D1, D2).
