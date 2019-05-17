-module(clj_vector).

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-export([ new/0
        , new/1
        , empty/0
        ]).

-export([ cons/2
        , reduce/2
        , reduce/3
        , get/2
        , set/3
        , size/1
        , to_list/1
        , tuple_for/2
        , pop/1
        ]).

-define(NODE_SIZE, 32).
-define(MASK, (?NODE_SIZE - 1)).
-define(SHIFT, 5).
-define(EMPTY_NODE, { ?NIL, ?NIL, ?NIL, ?NIL
                    , ?NIL, ?NIL, ?NIL, ?NIL
                    , ?NIL, ?NIL, ?NIL, ?NIL
                    , ?NIL, ?NIL, ?NIL, ?NIL
                    , ?NIL, ?NIL, ?NIL, ?NIL
                    , ?NIL, ?NIL, ?NIL, ?NIL
                    , ?NIL, ?NIL, ?NIL, ?NIL
                    , ?NIL, ?NIL, ?NIL, ?NIL
                    }
       ).

-define( EMPTY
       , #clj_vector{ size    = 0
                    , shift   = ?SHIFT
                    , root    = ?EMPTY_NODE
                    , tail    = ?EMPTY_NODE
                    , tailoff = 0
                    }
       ).

-record(clj_vector, { size   = 0      :: size()
                    , shift  = ?SHIFT :: shift()
                    , root            :: tree_node()
                    , tail            :: tree_node()
                    , tailoff = 0     :: offset()
                    }).

-type size()      :: non_neg_integer().
-type index()     :: non_neg_integer().
-type shift()     :: non_neg_integer().
-type tree_node() :: tuple().
-type offset()    :: non_neg_integer().
-type vector()    :: #clj_vector{}.

new() ->
  new([]).

-spec new(list()) -> vector().
new([]) ->
  ?EMPTY;
new(Items) when is_list(Items) ->
  Length = length(Items),
  case Length =< ?NODE_SIZE of
    true ->
      IndexedValues = index_values_asc(Items, 1, []),
      Node = erlang:make_tuple(?NODE_SIZE, ?NIL, IndexedValues),
      cons_node(Node, Length, ?EMPTY);
    _ ->
      from_list([], ?NODE_SIZE, ?EMPTY, Items)
  end.

-spec from_list(list(), index(), vector(), list()) -> vector().
%% Node list is full with ?NODE_SIZE
from_list(_NodeList, ?NODE_SIZE, Vector, []) ->
  Vector;
from_list(NodeList, 0, Vector0, Items) ->
  Node = list_to_tuple(lists:reverse(NodeList)),
  Vector1 = cons_node(Node, ?NODE_SIZE, Vector0),
  from_list([], ?NODE_SIZE, Vector1, Items);
%% Node list is not full but there are no more items
from_list(NodeList, NodeSizeLeft, Vector, []) ->
  NodeSize = ?NODE_SIZE - NodeSizeLeft,
  IndexedValues = index_values(NodeList, NodeSize, []),
  Node = erlang:make_tuple(?NODE_SIZE, ?NIL, IndexedValues),
  cons_node(Node, NodeSize, Vector);
from_list(NodeList, NodeSizeLeft, Vector, [Item | Items]) ->
  from_list([Item | NodeList], NodeSizeLeft - 1, Vector, Items).

-spec index_values(list(), size(), list()) -> list().
index_values([], _, Acc) ->
  Acc;
index_values([X | Rest], N, Acc) ->
  index_values(Rest, N - 1, [{N, X} | Acc]).

-spec index_values_asc(list(), size(), list()) -> list().
index_values_asc([], _, Acc) ->
  Acc;
index_values_asc([X | Rest], N, Acc) ->
  index_values_asc(Rest, N + 1, [{N, X} | Acc]).

-spec empty() -> vector().
empty() -> ?EMPTY.

-spec size(vector()) -> size().
size(#clj_vector{size = Size}) -> Size.

-spec reduce(function(), vector()) -> any().
reduce(F, #clj_vector{size = 0}) ->
  clj_rt:apply(F, []);
reduce(_F, #clj_vector{size = 1} = Vector) ->
  get(0, Vector);
reduce( F
      , #clj_vector{ size    = Size
                   , shift   = Shift
                   , root    = Root
                   , tail    = Tail
                   , tailoff = TailOffset
                   }
      ) ->
  Init = element(1, tuple_for(0, Shift, Root, Tail, TailOffset)),
  reduce_loop(F, Init, 1, Size, Shift, Root, Tail, TailOffset).

-spec reduce(function(), any(), vector()) -> any().
reduce(_F, Init, #clj_vector{size = 0}) ->
  Init;
reduce( F
      , Init
      , #clj_vector{ size    = Size
                   , shift   = Shift
                   , root    = Root
                   , tail    = Tail
                   , tailoff = TailOffset
                   }
      ) ->
  reduce_loop(F, Init, 0, Size, Shift, Root, Tail, TailOffset).

-spec to_list(vector()) -> [any()].
to_list(#clj_vector{size = 0}) ->
  [];
to_list(#clj_vector{ size    = Size
                   , shift   = Shift
                   , root    = Root
                   , tail    = Tail
                   , tailoff = TailOffset
                   }
       ) ->
  case Size =< ?NODE_SIZE of
    true  -> to_list_loop_tuple([], Tail, Size);
    false -> to_list_loop([], Size - 1, Shift, Root, Tail, TailOffset)
  end.

-spec tuple_for(index(), vector()) -> tree_node().
tuple_for( Index
         , #clj_vector{ shift   = Shift
                      , root    = Root
                      , tail    = Tail
                      , tailoff = TailOffset
                      }) ->
  tuple_for(Index, Shift, Root, Tail, TailOffset).

-spec cons(any(), vector()) -> vector().
cons( Value
    , #clj_vector{ size    = Size
                 , shift   = Shift
                 , root    = Root
                 , tail    = Tail
                 , tailoff = TailOffset
                 } = Vector
    ) ->
  case Size - TailOffset of
    %% Space available in the tail
    Idx when Idx < ?NODE_SIZE ->
      Vector#clj_vector{ size = Size + 1
                       , tail = setelement(Idx + 1, Tail, Value)
                       };
    %% Root overflow
    _ when (Size bsr ?SHIFT) > (1 bsl Shift) ->
      NewPath = new_path(Shift, Tail),
      InitValues = [{1, Root}, {2, NewPath}],
      NewRoot = erlang:make_tuple(?NODE_SIZE, ?NIL, InitValues),
      NewTail = setelement(1, Tail, Value),
      #clj_vector{ size    = Size + 1
                 , shift   = Shift + ?SHIFT
                 , root    = NewRoot
                 , tail    = NewTail
                 , tailoff = tailoff(Size + 1)
                 };
    %% Otherwise just push the tail
    _ ->
      NewRoot = push_tail(Size, Shift, Root, Tail),
      NewTail = setelement(1, Tail, Value),
      #clj_vector{ size  = Size + 1
                 , shift = Shift
                 , root  = NewRoot
                 , tail  = NewTail
                 , tailoff = tailoff(Size + 1)
                 }
  end.

-spec get(index(), vector()) -> any().
get( Index
   , #clj_vector{ size    = Size
                , shift   = Shift
                , root    = Root
                , tail    = Tail
                , tailoff = TailOffset
                }
   ) when is_integer(Index), Index >= 0, Index < Size ->
  case Index >= TailOffset of
    true  ->
      Idx = Index band ?MASK,
      element(Idx + 1, Tail);
    false ->
      get_from_node(Index, Shift, Root)
  end;
get(_Index, _Vector) ->
  ?ERROR(<<"Index out of bounds">>).

-spec set(index(), any(), vector()) -> vector().
set( Index
   , Value
   , #clj_vector{ size    = Size
                , shift   = Shift
                , root    = Root
                , tail    = Tail
                , tailoff = TailOffset
                } = Vector
   ) when is_integer(Index), Index >= 0, Index < Size ->
  case Index >= TailOffset of
    true ->
      Idx = Index band ?MASK + 1,
      Vector#clj_vector{tail = setelement(Idx, Tail, Value)};
    false ->
      Vector#clj_vector{root = set_new_root(Shift, Root, Index, Value)}
  end;
set( Index
   , Value
   , #clj_vector{size  = Size} = Vector
   ) when Index =:= Size ->
  cons(Value, Vector);
set(_Index, _Value, _Vector) ->
  ?ERROR(<<"Index out of bounds">>).

-spec pop(vector()) -> vector().
pop(#clj_vector{size = 0}) ->
  ?ERROR(<<"Can't pop empty vector">>);
pop(#clj_vector{size = 1}) ->
  ?EMPTY;
%% Pop from tail
pop( #clj_vector{ size    = Size
                , tailoff = TailOffset
                } = Vector
   ) when Size - TailOffset > 1 ->
  Vector#clj_vector{size = Size - 1};
%% Replace empty tail
pop( #clj_vector{ size    = Size0
                , shift   = Shift0
                , root    = Root0
                } = Vector
   ) ->
  Tail1 = tuple_for_loop(Size0 - 2, Shift0, Root0),
  Root1 = case pop_tail(Size0, Shift0, Root0) of
              ?NIL -> ?EMPTY_NODE;
              X -> X
            end,
  {Root2, Shift1} = case Shift0 > 5 andalso element(2, Root1) =:= ?NIL of
               true  -> {element(1, Root1), Shift0 - 5};
               false -> {Root1, Shift0}
             end,
  Size1 = Size0 - 1,
  Vector#clj_vector{ size    = Size1
                   , shift   = Shift1
                   , root    = Root2
                   , tail    = Tail1
                   , tailoff = tailoff(Size1)
                   }.

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

-spec new_path(shift(), tree_node()) -> tree_node().
new_path(_Level = 0, Node) ->
  Node;
new_path(Level,  Node) ->
  NewNode = erlang:make_tuple(?NODE_SIZE, ?NIL),
  setelement(1, NewNode, new_path(Level - ?SHIFT,  Node)).

-spec push_tail(size(), shift(), tree_node(), tree_node()) ->
  tree_node().
push_tail(Size, ?SHIFT, Parent, Tail) ->
  Idx = ((Size - 1) bsr ?SHIFT) band ?MASK + 1,
  setelement(Idx, Parent, Tail);
push_tail(Size, Level, Parent, Tail) ->
  Idx = ((Size - 1) bsr Level) band ?MASK + 1,
  NewNode = case element(Idx, Parent) of
              ?NIL -> new_path(Level - ?SHIFT, Tail);
              Child -> push_tail(Size, Level - ?SHIFT, Child, Tail)
            end,
  setelement(Idx, Parent, NewNode).

-spec tailoff(size()) -> offset().
tailoff(Size) when Size < ?NODE_SIZE ->
  0;
tailoff(Size) ->
  ((Size - 1) bsr ?SHIFT) bsl ?SHIFT.

-spec get_from_node(index(), shift(), tree_node()) ->
  any().
get_from_node(Index, 0, Node) ->
  Idx = Index band (?NODE_SIZE - 1),
  element(Idx + 1, Node);
get_from_node(Index, Level, Node) ->
  Idx = (Index bsr Level) band ?MASK + 1,
  get_from_node(Index, Level - ?SHIFT, element(Idx, Node)).

-spec set_new_root(shift(), tree_node(), index(), any()) ->
  tree_node().
set_new_root(_Level = 0, Node, Index, Value) ->
  setelement(Index band ?MASK + 1, Node, Value);
set_new_root(Level, Node, Index, Value) ->
  Idx = (Index bsr Level) band ?MASK + 1,
  Child = set_new_root(Level - ?SHIFT, element(Idx, Node), Index, Value),
  setelement(Idx, Node, Child).

-spec tuple_for(index(), shift(), tree_node(), tree_node(), offset()) ->
  tree_node().
tuple_for(Index, _Shift, _Root, Tail, TailOffset) when Index >= TailOffset ->
  Tail;
tuple_for(Index, Shift, Root, _Tail, _TailOffset) ->
  tuple_for_loop(Index, Shift, Root).

-spec tuple_for_loop(index(), shift(), tree_node()) -> tree_node().
tuple_for_loop(_Index, 0, Node)->
  Node;
tuple_for_loop(Index, Level, Node) ->
  Idx = (Index bsr Level) band ?MASK + 1,
  tuple_for_loop(Index, Level - ?SHIFT, element(Idx, Node)).

-spec pop_tail(size(), shift(), tree_node()) -> ?NIL | tree_node().
pop_tail(Size, Level, Node) ->
  case ((Size - 2) bsr Level) band ?MASK of
    0 -> ?NIL;
    Idx when Level > 5 ->
      NewChild = pop_tail(Size, Level - ?SHIFT, element(Idx + 1, Node)),
      setelement(Idx + 1, Node, NewChild);
    Idx ->
      setelement(Idx + 1, Node, ?NIL)
  end.

-spec reduce_loop( any(), any(), index(), size(), shift()
                 , tree_node(), tree_node(), offset()
                 ) -> any().
reduce_loop(F, Acc, CurrentIndex, Size, _Shift, _Root, Tail, TailOffset)
  when CurrentIndex >= TailOffset ->
  StartPos = CurrentIndex band ?MASK + 1,
  EndPos   = Size - TailOffset,
  case reduce_loop_tuple(F, Acc, Tail, StartPos, EndPos) of
    {plain, Val} -> Val;
    {reduced, Reduced} -> 'clojerl.Reduced':deref(Reduced)
  end;
reduce_loop(F, Acc0, CurrentIndex, Size, Shift, Root, Tail, TailOffset) ->
  Tuple      = tuple_for_loop(CurrentIndex, Shift, Root),
  StartIndex = CurrentIndex band ?MASK,
  NextIndex  = CurrentIndex + ?NODE_SIZE - StartIndex,
  StartPos   = StartIndex + 1,
  case reduce_loop_tuple(F, Acc0, Tuple, StartPos, ?NODE_SIZE) of
    {plain, Acc1} ->
      reduce_loop(F, Acc1, NextIndex, Size, Shift, Root, Tail, TailOffset);
    {reduced, Reduced} ->
      'clojerl.Reduced':deref(Reduced)
  end.

-spec reduce_loop_tuple(any(), any(), tree_node(), index(), index()) ->
  any().
reduce_loop_tuple(_F, Acc, _Tuple, Current, End) when Current > End ->
  {plain, Acc};
reduce_loop_tuple(F, Acc0, Tuple, Current, End) ->
  Acc1 = clj_rt:apply(F, [Acc0, element(Current, Tuple)]),
  case 'clojerl.Reduced':is_reduced(Acc1) of
    true  -> {reduced, Acc1};
    false -> reduce_loop_tuple(F, Acc1, Tuple, Current + 1, End)
  end.

-spec to_list_loop( list(), index(), shift(), tree_node()
                  , tree_node(), offset()
                  ) -> list().
to_list_loop(List, Index, _Shift, _Root, _Tail, _TailOffset)
  when Index < 0 ->
  List;
to_list_loop(List, Index, Shift, Root, Tail, TailOffset)
  when Index >= TailOffset ->
  Position = Index band ?MASK + 1,
  List1 = to_list_loop_tuple(List, Tail, Position),
  to_list_loop(List1,  Index - Position, Shift, Root, Tail, TailOffset);
to_list_loop(List, Index, Shift, Root, Tail, TailOffset) ->
  Node = tuple_for_loop(Index, Shift, Root),
  List1 = to_list_loop_tuple(List, Node, ?NODE_SIZE),
  to_list_loop(List1,  Index - ?NODE_SIZE, Shift, Root, Tail, TailOffset).

-spec to_list_loop_tuple(list(), tree_node(), index()) -> list().
to_list_loop_tuple(List, _Node, 0) ->
  List;
to_list_loop_tuple(List, Node, Current) ->
  to_list_loop_tuple([element(Current, Node) | List], Node, Current - 1).

-spec cons_node(any(), size(), vector()) -> vector().
cons_node(Node, NodeSize, #clj_vector{size = 0} = Vector) ->
  %% Vector is empty, the node is the tail
  Vector#clj_vector{size = NodeSize, tail = Node};
cons_node( Node
         , NodeSize
         , #clj_vector{ size    = Size
                      , shift   = Shift
                      , root    = Root
                      , tail    = Tail
                      }
         ) when (Size bsr ?SHIFT) > (1 bsl Shift) ->
  %% Root overflow
  NewPath = new_path(Shift, Tail),
  InitValues = [{1, Root}, {2, NewPath}],
  NewRoot = erlang:make_tuple(?NODE_SIZE, ?NIL, InitValues),
  #clj_vector{ size    = Size + NodeSize
             , shift   = Shift + ?SHIFT
             , root    = NewRoot
             , tail    = Node
             , tailoff = tailoff(Size + NodeSize)
             };
cons_node( Node
         , NodeSize
         , #clj_vector{ size    = Size
                      , shift   = Shift
                      , root    = Root
                      , tail    = Tail
                      } = Vector
         ) ->
  %% Otherwise just push the tail
  NewRoot = push_tail(Size, Shift, Root, Tail),
  Vector#clj_vector{ size  = Size + NodeSize
                   , root  = NewRoot
                   , tail  = Node
                   , tailoff = tailoff(Size + NodeSize)
                   }.
