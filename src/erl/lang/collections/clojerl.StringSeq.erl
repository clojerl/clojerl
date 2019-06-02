-module('clojerl.StringSeq').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-behavior('clojerl.ICounted').
-behavior('clojerl.IColl').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IHash').
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
-export([ first/1
        , next/1
        , more/1
        ]).
-export(['_'/1]).
-export([ seq/1
        , to_list/1
        ]).
-export([str/1]).

-type type() :: #{ ?TYPE => ?M
                 , str   => binary()
                 }.

-spec ?CONSTRUCTOR(list()) -> type().
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
  clj_murmur3:ordered(Str).

%% clojerl.ISeq

first(#{?TYPE := ?M, str := <<>>}) -> ?NIL;
first(#{?TYPE := ?M, str := <<Ch/utf8, _/binary>>}) -> <<Ch/utf8>>.

next(#{?TYPE := ?M, str := <<>>}) -> ?NIL;
next(#{?TYPE := ?M, str := <<_/utf8>>}) -> ?NIL;
next(#{?TYPE := ?M, str := <<_/utf8, Rest/binary>>} = Seq) ->
  Seq#{str => Rest}.

more(#{?TYPE := ?M, str := <<>>}) -> ?NIL;
more(#{?TYPE := ?M, str := <<_/utf8, Rest/binary>>} = Seq) ->
  Seq#{str => Rest}.

%% clojerl.ISequential

'_'(_) -> ?NIL.

%% clojerl.ISeqable

seq(#{?TYPE := ?M, str := <<>>}) -> ?NIL;
seq(#{?TYPE := ?M} = Seq) -> Seq.

to_list(#{?TYPE := ?M, str := Str})  -> to_list(Str, []).

%% clojerl.IStringable

str(#{?TYPE := ?M, str := Str}) ->
  clj_rt:print_str(to_list(Str, [])).

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

to_list(<<>>, Result) ->
  lists:reverse(Result);
to_list(<<Ch/utf8, Rest/binary>>, Result) ->
  to_list(Rest, [<<Ch/utf8>> | Result]).
