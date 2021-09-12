%% @doc Wraps a string providing an implementation for collection and
%% seq related protocols.

%% @private
-module('clojerl.StringSeq').

-include("clojerl.hrl").

-behavior('clojerl.ICounted').
-behavior('clojerl.IColl').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IHash').
-behavior('clojerl.IReduce').
-behavior('clojerl.ISeq').
-behavior('clojerl.ISeqable').
-behavior('clojerl.ISequential').
-behavior('clojerl.IStringable').

-export([?CONSTRUCTOR/1]).

-export([count/1]).
-export([ cons/2
        , empty/1
        ]).
-export([equiv/2]).
-export([hash/1]).
-export([ reduce/2
        , reduce/3
        ]).
-export([ first/1
        , next/1
        , more/1
        ]).
-export([ seq/1
        , to_list/1
        ]).
-export([str/1]).

-export_type([type/0]).
-type type() :: #{ ?TYPE => ?M
                 , str   => binary()
                 }.

-spec ?CONSTRUCTOR(binary()) -> type().
?CONSTRUCTOR(Str) ->
  #{ ?TYPE => ?M
   , str   => Str
   }.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

%% clojerl.ICounted

count(#{?TYPE := ?M, str := Str}) ->
  'clojerl.String':length(Str).

%% clojerl.IColl

cons(#{?TYPE := ?M} = Seq, X) ->
  'clojerl.Cons':?CONSTRUCTOR(X, Seq).

empty(_) -> [].

%% clojerl.IEquiv

equiv( #{?TYPE := ?M, str := Str}
     , #{?TYPE := ?M, str := Str}
     ) ->
  true;
equiv(#{?TYPE := ?M, str := X}, Y) ->
  case clj_rt:'sequential?'(Y) of
    true  ->
      List = 'clojerl.String':to_list(X),
      'erlang.List':equiv(List, Y);
    false -> false
  end.

%% clojerl.IHash

hash(#{?TYPE := ?M, str := Str}) ->
  clj_murmur3:ordered(to_list(Str, [])).

%% clojerl.IReduce

reduce(#{?TYPE := ?M, str := <<>>}, F) ->
  clj_rt:apply(F, []);
reduce(#{?TYPE := ?M, str := <<First/utf8, Rest/binary>>}, F) ->
  do_reduce(F, <<First/utf8>>, Rest).

reduce(#{?TYPE := ?M, str := Str}, F, Init) ->
  do_reduce(F, Init, Str).

do_reduce(F, Acc, <<First/utf8, Rest/binary>>) ->
  Val = clj_rt:apply(F, [Acc, <<First/utf8>>]),
  case 'clojerl.Reduced':is_reduced(Val) of
    true  -> 'clojerl.Reduced':deref(Val);
    false -> do_reduce(F, Val, Rest)
  end;
do_reduce(_F, Acc, <<>>) ->
  Acc.

%% clojerl.ISeq

first(#{?TYPE := ?M, str := <<>>}) -> ?NIL;
first(#{?TYPE := ?M, str := <<Ch/utf8, _/binary>>}) -> <<Ch/utf8>>.

next(#{?TYPE := ?M, str := <<>>}) -> ?NIL;
next(#{?TYPE := ?M, str := <<_/utf8>>}) -> ?NIL;
next(#{?TYPE := ?M, str := <<_/utf8, Rest/binary>>} = Seq) ->
  Seq#{str => Rest}.

more(#{?TYPE := ?M, str := <<>>}) -> [];
more(#{?TYPE := ?M, str := <<_/utf8, Rest/binary>>} = Seq) ->
  Seq#{str => Rest}.

%% clojerl.ISeqable

seq(#{?TYPE := ?M, str := <<>>}) -> ?NIL;
seq(#{?TYPE := ?M} = Seq) -> Seq.

to_list(#{?TYPE := ?M, str := Str}) -> to_list(Str, []).

%% clojerl.IStringable

str(#{?TYPE := ?M, str := Str}) ->
  clj_rt:print_str(clj_rt:list(to_list(Str, []))).

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

to_list(<<>>, Result) ->
  lists:reverse(Result);
to_list(<<Ch/utf8, Rest/binary>>, Result) ->
  to_list(Rest, [<<Ch/utf8>> | Result]).
