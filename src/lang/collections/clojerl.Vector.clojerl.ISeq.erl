-module('clojerl.Vector.clojerl.ISeq').

-behaviour('clojerl.ISeq').

-export([
         first/1,
         next/1,
         more/1
        ]).

-spec first('clojerl.Vector':type()) -> undefined | any().
first({_, Array, _}) ->
  case array:size(Array) of
    0 -> undefined;
    _ -> array:get(0, Array)
  end.

-spec next('clojerl.Vector':type()) -> undefined | 'clojerl.List':type().
next({_, Array, _}) ->
  case array:size(Array) of
    0 -> undefined;
    _ ->
      %% Reset the first element and get all non-resset elements.
      RestArray = array:reset(0, Array),
      Items = array:sparse_to_list(RestArray),
      'clojerl.List':new(Items)
  end.

-spec more('clojerl.Vector':type()) -> undefined | 'clojerl.List':type().
more({_, Array, _} = Vector) ->
  case array:size(Array) of
    0 -> 'clojerl.List':new([]);
    _ -> next(Vector)
  end.
