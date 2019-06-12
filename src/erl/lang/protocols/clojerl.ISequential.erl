-module('clojerl.ISequential').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export([?SATISFIES/1]).
-export([?EXTENDS/1]).

-callback '_'(any()) -> any().

?SATISFIES(X) ->
  case X of
    #{?TYPE := 'clojerl.Iterate'} ->  true;
    #{?TYPE := 'clojerl.Range'} ->  true;
    #{?TYPE := 'clojerl.Vector.ChunkedSeq'} ->  true;
    #{?TYPE := 'clojerl.List'} ->  true;
    #{?TYPE := 'clojerl.LazySeq'} ->  true;
    #{?TYPE := 'clojerl.TransducerSeq'} ->  true;
    #{?TYPE := 'clojerl.Vector'} ->  true;
    #{?TYPE := 'clojerl.Vector.RSeq'} ->  true;
    #{?TYPE := 'clojerl.Cycle'} ->  true;
    #{?TYPE := 'clojerl.Repeat'} ->  true;
    #{?TYPE := 'clojerl.Cons'} ->  true;
    #{?TYPE := 'clojerl.ChunkedCons'} ->  true;
    #{?TYPE := _} ->  false;
    X_ when erlang:is_binary(X_) ->  false;
    X_ when erlang:is_bitstring(X_) ->  true;
    X_ when erlang:is_boolean(X_) ->  false;
    X_ when erlang:is_list(X_) ->  true;
    X_ when erlang:is_tuple(X_) ->  true;
    ?NIL ->  false;
    _ -> false
  end.

?EXTENDS(X) ->
  case X of
    'clojerl.Iterate' -> true;
    'clojerl.Range' -> true;
    'clojerl.Vector.ChunkedSeq' -> true;
    'clojerl.List' -> true;
    'clojerl.LazySeq' -> true;
    'clojerl.TransducerSeq' -> true;
    'clojerl.Vector' -> true;
    'clojerl.Vector.RSeq' -> true;
    'clojerl.Cycle' -> true;
    'clojerl.Repeat' -> true;
    'clojerl.Cons' -> true;
    'clojerl.ChunkedCons' -> true;
    'clojerl.BitString' -> true;
    'erlang.List' -> true;
    'erlang.Tuple' -> true;
    _ -> false
  end.
