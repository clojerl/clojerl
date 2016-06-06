-module(clj_murmur3).

-export([ ordered/1
        , unordered/1
        ]).

%% Exported to avoid xref warning.
-export([init/0]).

-on_load(init/0).

-define(APPNAME, clojerl).

-spec ordered(any()) -> integer().
ordered(Seq) ->
  List = clj_core:seq_to_list(Seq),
  Hashes = ['clojerl.IHash':hash(X) || X <- List],
  ordered_nif(Hashes).

-spec unordered(any()) -> integer().
unordered(Seq) ->
  List = clj_core:seq_to_list(Seq),
  Hashes = ['clojerl.IHash':hash(X) || X <- List],
  unordered_nif(Hashes).

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

init() ->
  SoName = case code:priv_dir(?APPNAME) of
             {error, bad_name} ->
               case filelib:is_dir(filename:join(["..", priv])) of
                 true ->
                   filename:join(["..", priv, ?APPNAME]);
                 _ ->
                   filename:join([priv, ?APPNAME])
               end;
             Dir ->
               filename:join(Dir, ?APPNAME)
           end,
  erlang:load_nif(SoName, 0).

ordered_nif(_) ->
  erlang:nif_error({error, not_loaded}).

unordered_nif(_) ->
  erlang:nif_error({error, not_loaded}).
