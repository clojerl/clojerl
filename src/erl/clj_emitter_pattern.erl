-module(clj_emitter_pattern).

-export([ patterns/1
        , patterns/2
        , fold_guards/1
        , fold_guards/2
        ]).

-include_lib("compiler/src/core_parse.hrl").

-import(ordsets, [is_element/2, union/2, intersection/2, subtract/2]).

fold_guards([]) ->
  cerl:abstract(true);
fold_guards([Guard0]) ->
  Guard0;
fold_guards([Guard0 | PatternGuards]) ->
  fold_guards(Guard0, PatternGuards).

fold_guards(Guard0, PatternGuards) ->
  FoldGuards = fun(PatGuard, Guard) ->
                   Ann = cerl:get_ann(Guard),
                   cerl:ann_c_call(Ann
                                  , cerl:c_atom(erlang)
                                  , cerl:c_atom('and')
                                  , [PatGuard, Guard]
                                  )
               end,
  lists:foldr(FoldGuards, Guard0, PatternGuards).

-spec patterns([cerl:cerl()]) ->
  {[cerl:cerl()], [cerl:cerl()]}.
patterns(Patterns) ->
  patterns(Patterns, []).

-spec patterns([cerl:cerl()], ordsets:ordset(atom())) ->
  {[cerl:cerl()], [cerl:cerl()]}.
patterns(Patterns, KnownVars) ->
  KnownVarsSet = ordsets:from_list(KnownVars),
  {PatArgs, PatGuards, _, _} = pattern_list(Patterns, KnownVarsSet),
  {PatArgs, PatGuards}.

%% -----------------------------------------------------------------------------
%% All of the code that follows was copied from the `v3_core'
%% module in the Erlang/OTP `compiler' application.
%%
%% Given a list of patterns, it generates the guards that apply to all of
%% them when considered together and it also transforms the patterns.
%% -----------------------------------------------------------------------------

pattern(#c_var{name='_', anno=Ann}, _) ->
  New = clj_emitter:new_c_var(Ann),
  {New, [], [New#c_var.name], []};
pattern(#c_var{name=V, anno=Ann}=Var, Ks) ->
  case is_element(V, Ks) of
    true ->
      New = clj_emitter:new_c_var(Ann),
      Test = #c_call{anno=Ann,
                     module=#c_literal{val=erlang},
                     name=#c_literal{val='=:='},
                     args=[New, Var]},
      %% Test doesn't need protecting.
      {New, [Test], [New#c_var.name], []};
    false -> {Var, [], [V], []}
  end;
pattern(#c_cons{hd=H0, tl=T0}=Cons, Ks) ->
  {H1, Hg, Hv, Hu} = pattern(H0, Ks),
  {T1, Tg, Tv, Tu} = pattern(T0, union(Hv, Ks)),
  {Cons#c_cons{hd=H1, tl=T1}, Hg ++ Tg, union(Hv, Tv), union(Hu, Tu)};
pattern(#c_tuple{es=Es0}=Tuple, Ks) ->
  {Es1, Esg, Esv, Eus} = pattern_list(Es0, Ks),
  {Tuple#c_tuple{es=Es1}, Esg, Esv, Eus};
pattern(#c_map{es=Es0}=Map, Ks) ->
  {Es1, Esg, Esv, Eus} = pattern_list(Es0, Ks),
  {Map#c_map{es=Es1}, Esg, Esv, Eus};
pattern(#c_map_pair{op=#c_literal{val=exact}, key=K0, val=V0}=Pair, Ks) ->
  {V, Vg, Vn, Vu} = pattern(V0, Ks),
  %% A variable key must be considered used here
  Ku = case K0 of
         #c_var{name=Name} -> [Name];
         _ -> []
       end,
  {Pair#c_map_pair{val=V}, Vg, Vn, union(Ku, Vu)};
pattern(#c_binary{segments=Es0}=Bin, Ks) ->
  {Es1, Esg, Esv, Eus} = pat_bin(Es0, Ks),
  {Bin#c_binary{segments=Es1}, Esg, Esv, Eus};
pattern(#c_alias{var=V0, pat=P0}=Alias, Ks) ->
  {V1, Vg, Vv, Vu} = pattern(V0, Ks),
  {P1, Pg, Pv, Pu} = pattern(P0, union(Vv, Ks)),
  {Alias#c_alias{var=V1, pat=P1}, Vg ++ Pg, union(Vv, Pv), union(Vu, Pu)};
pattern(Other, _) -> {Other, [], [], []}. %Constants

%% pattern_list([Pat], [KnownVar], State) ->
%%                        {[Pat], [GuardTest], [NewVar], [UsedVar], State}.

pattern_list([P0|Ps0], Ks) ->
  {P1, Pg, Pv, Pu} = pattern(P0, Ks),
  {Ps1, Psg, Psv, Psu} = pattern_list(Ps0, union(Pv, Ks)),
  {[P1|Ps1], Pg ++ Psg, union(Pv, Psv), union(Pu, Psu)};
pattern_list([], _) -> {[], [], [], []}.

%% pat_bin([Pat], [KnownVar]) ->
%%                        {[Pat], [GuardTest], [NewVar], [UsedVar]}.
pat_bin(Es0, Ks) ->
  {Es1, Pg, Pv, Pu0} = pat_bin(Es0, Ks, []),

  %% In a clause such as <<Sz:8, V:Sz>> in a function head, Sz will both
  %% be new and used; a situation that is not handled properly by
  %% uclause/4.  (Basically, since Sz occurs in two sets that are
  %% subtracted from each other, Sz will not be added to the list of
  %% known variables and will seem to be new the next time it is
  %% used in a match.)
  %%   Since the variable Sz really is new (it does not use a
  %% value bound prior to the binary matching), Sz should only be
  %% included in the set of new variables. Thus we should take it
  %% out of the set of used variables.

  Pu1 = subtract(Pu0, intersection(Pv, Pu0)),
  {Es1, Pg, Pv, Pu1}.

%% pat_bin([Pat], [KnownVar], [LocalVar], State) ->
%%                        {[Pat], [GuardTest], [NewVar], [UsedVar], State}.
pat_bin([P0|Ps0], Ks, Bs) ->
  {P1, Pg, Pv, Pu, Bs1} = pat_element(P0, Ks, Bs),
  {Ps1, Psg, Psv, Psu} = pat_bin(Ps0, union(Pv, Ks), Bs1),
  {[P1|Ps1], Pg ++ Psg, union(Pv, Psv), union(Pu, Psu)};
pat_bin([], _, _) -> {[], [], [], []}.

%% pat_element(Segment, [KnownVar], [LocalVar], State) ->
%%        {Segment, [GuardTest], [NewVar], [UsedVar], [LocalVar], State}
pat_element(#c_bitstr{val=H0, size=Sz0}=Seg, Ks, Bs0) ->
  {H1, Hg, Hv, []} = pattern(H0, Ks),
  Bs1 = case H0 of
          #c_var{name=Hname} ->
            case H1 of
              #c_var{name=Hname} ->
                Bs0;
              #c_var{name=Other} ->
                [{Hname, Other}|Bs0]
            end;
          _ ->
            Bs0
        end,
  {Sz1, Us} = case Sz0 of
               #c_var{name=Vname} ->
                 rename_bitstr_size(Vname, Bs0);
               _Other ->
                 {Sz0, []}
             end,
  {Seg#c_bitstr{val=H1, size=Sz1}, Hg, Hv, Us, Bs1}.

rename_bitstr_size(V, [{V, N}|_]) ->
  New = #c_var{name=N},
  {New, [N]};
rename_bitstr_size(V, [_|Rest]) ->
  rename_bitstr_size(V, Rest);
rename_bitstr_size(V, []) ->
  Old = #c_var{name=V},
  {Old, [V]}.
