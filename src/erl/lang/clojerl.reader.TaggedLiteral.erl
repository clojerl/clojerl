-module('clojerl.reader.TaggedLiteral').

-include("clojerl.hrl").

-behavior('clojerl.IEquiv').
-behavior('clojerl.ILookup').
-behavior('clojerl.IStringable').

-export([?CONSTRUCTOR/2]).
-export([equiv/2]).
-export([ get/2
        , get/3
        ]).
-export([str/1]).

-type type() :: #{ ?TYPE => ?M
                 , tag   => 'clojerl.Symbol':type()
                 , form  => any()
                 }.

-spec ?CONSTRUCTOR('clojerl.Symbol':type(), any()) -> type().
?CONSTRUCTOR(Tag, Form) ->
  #{ ?TYPE => ?M
   , tag   => Tag
   , form  => Form
   }.

%% clojerl.IEquiv

equiv( #{?TYPE := ?M, tag := T1, form := F1}
     , #{?TYPE := ?M, tag := T2, form := F2}
     ) ->
  clj_rt:equiv(T1, T2) andalso clj_rt:equiv(F1, F2);
equiv(_, _) ->
  false.

%% clojerl.ILookup

get(#{?TYPE := ?M} = TaggedLiteral, Key) ->
  get(TaggedLiteral, Key, ?NIL).

get(#{?TYPE := ?M, tag := Tag}, tag, _) ->
  Tag;
get(#{?TYPE := ?M, form := Form}, form, _) ->
  Form;
get(#{?TYPE := ?M}, _, NotFound) ->
  NotFound.

%% clojerl.IStringable

str(#{?TYPE := ?M, tag := Tag, form := Form}) ->
  TagBin  = clj_rt:str(Tag),
  FormBin = clj_rt:str(Form),
  <<"#", TagBin/binary, " ", FormBin/binary>>.
