-module(clj_env).

-export([
         default/0,
         add_expr/2,
         in_ns/2,
         add_ns/2,
         current_ns/1,
         current_ns/2,
         update_ns/3,
         get_ns/2
        ]).

-type env() :: #{current_ns => 'clojerl.Symbol':type(),
                 local_bindings => #{'clojerl.Symbol':type() => any()},
                 exprs => [],
                 namespaces => []}.

-export_type([env/0]).

-spec default() -> env().
default() ->
  UserSym = clj_core:symbol(user),
  UserNs = clj_namespace:new(UserSym),
  #{namespaces     => #{UserSym => UserNs},
    exprs          => [],
    current_ns     => clj_core:symbol(user),
    local_bindings => #{}}.

-spec add_expr(env(), erl_syntax:syntaxTree()) -> env().
add_expr(Env = #{exprs := Exprs}, Expr) ->
  Env#{exprs => [Expr | Exprs]};
add_expr(Env, Expr) ->
  Env#{exprs => [Expr]}.

-spec in_ns('clojerl.Symbol':type(), env()) -> env().
in_ns(NsSym, Env) ->
  case get_ns(NsSym, Env) of
    undefined ->
      Ns = clj_namespace:new(NsSym),
      current_ns(NsSym, add_ns(Ns, Env));
    _ ->
      current_ns(NsSym, Env)
  end.

-spec add_ns(clj_namespace:namespace(), env()) -> env().
add_ns(Ns, Env = #{namespaces := Namespaces}) ->
  NsSym = clj_namespace:name(Ns),
  case get_ns(NsSym, Env) of
    undefined ->
      NewNamespaces = maps:put(NsSym, Ns, Namespaces),
      Env#{namespaces => NewNamespaces};
    _ ->
      Env
  end.

-spec current_ns(env()) -> clj_namespace:namespace().
current_ns(#{current_ns := CurrentNs}) ->
  CurrentNs.

-spec current_ns('clojerl.Symbol':type(), env()) -> env().
current_ns(CurrentNs, Env) ->
  case get_ns(CurrentNs, Env) of
    undefined ->
      throw(<<"The specified namespace does not exist">>);
    _ ->
      Env#{current_ns => CurrentNs}
  end.

-spec update_ns('clojerl.Symbol':type(), function(), env()) -> clj_namespace:namespace().
update_ns(Name, Fun, Env = #{namespaces := Nss}) ->
  case maps:get(Name, Nss, undefined) of
    undefined ->
      Env;
    Ns ->
      NewNs = Fun(Ns),
      NewNss = maps:put(Name, NewNs, Nss),
      Env#{namespaces => NewNss}
  end.

-spec get_ns('clojerl.Symbol':type(), env()) -> clj_namespace:namespace().
get_ns(SymNs, _Env = #{namespaces := Nss}) ->
  maps:get(SymNs, Nss, undefined).
