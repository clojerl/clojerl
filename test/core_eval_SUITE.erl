-module(core_eval_SUITE).

-include("clj_test_utils.hrl").

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        ]).

-export([ alias/1
        , apply/1
        , binary/1
        , call/1
        , 'case'/1
        , cons/1
        , 'fun'/1
        , letrec/1
        , literal/1
        , map/1
        , primop/1
        , 'receive'/1
        , seq/1
        , 'try'/1
        , tuple/1
        , values/1
        , var/1
        , invalid_exprs/1
        , complete_coverage/1
        ]).

-spec all() -> [atom()].
all() -> clj_test_utils:all(?MODULE).

-spec init_per_suite(config()) -> config().
init_per_suite(Config) -> clj_test_utils:init_per_suite(Config).

-spec end_per_suite(config()) -> config().
end_per_suite(Config) -> Config.

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------

alias(_Config) ->
  Foo    = cerl:abstract(foo),
  Var    = cerl:c_var(x),

  ct:comment("Alias in a successful pattern match"),
  Alias  = cerl:c_alias(Var, Foo),
  Clause = cerl:c_clause([Alias], Var),
  Expr   = cerl:c_case(Foo, [Clause]),

  foo    = core_eval:expr(Expr),

  ct:comment("Alias in an unsuccessful pattern match"),
  Bar     = cerl:abstract(bar),
  Alias2  = cerl:c_alias(Var, Bar),
  Clause2 = cerl:c_clause([Alias2], Var),
  Expr2   = cerl:c_case(Foo, [Clause2]),

  {case_clause, foo} = try core_eval:expr(Expr2), ok
                       catch _:Error -> Error
                       end,

  ct:comment("Alias as an expression"),
  {illegal_expr, _} = try core_eval:expr(Alias)
                      catch _:InvalidExpr -> InvalidExpr
                      end,

  {comments, ""}.

apply(_Config) ->
  ct:comment("Apply fun"),
  Args  = [cerl:abstract(erlang), cerl:abstract('+'), cerl:abstract(2)],
  Plus  = cerl:c_call(cerl:abstract(erlang), cerl:abstract(make_fun), Args),
  One   = cerl:abstract(1),
  Apply = cerl:c_apply(Plus, [One, One]),

  2     = core_eval:expr(Apply),

  {comments, ""}.

binary(_Config) ->
  ct:comment("Binary"),
  SegmentA  = cerl:c_bitstr( cerl:abstract($a)
                           , cerl:abstract(8)
                           , cerl:abstract(1)
                           , cerl:abstract(utf8)
                           , cerl:abstract([])
                           ),
  Binary1   = cerl:c_binary([SegmentA, SegmentA, SegmentA]),
  <<"aaa">> = core_eval:expr(Binary1),

  ct:comment("Bitstring"),
  SegmentBit = cerl:c_bitstr( cerl:abstract(1)
                            , cerl:abstract(1)
                            , cerl:abstract(1)
                            , cerl:abstract(integer)
                            , cerl:abstract([])
                            ),
  Binary2    = cerl:c_binary([SegmentBit, SegmentBit, SegmentBit]),
  <<1:1, 1:1, 1:1>> = core_eval:expr(Binary2),

  {comments, ""}.

call(_Config) ->
  Args  = [cerl:abstract(1), cerl:abstract(1)],
  Plus  = cerl:c_call(cerl:abstract(erlang), cerl:abstract('+'), Args),
  2     = core_eval:expr(Plus),

  {comments, ""}.

'case'(_Config) ->
  ct:comment("Simple case with list pattern matching"),
  Arg1     = cerl:abstract([1, 2, 3]),

  VarX     = cerl:c_var(x),
  VarXs    = cerl:c_var(xs),
  Pattern1 = cerl:c_cons(VarX, VarXs),
  Clause1  = cerl:c_clause([Pattern1], VarX),

  Case1    = cerl:c_case(Arg1, [Clause1]),
  1        = core_eval:expr(Case1),

  {comments, ""}.

cons(_Config) ->
  ct:comment("Simple cons"),
  X          = cerl:c_var(x),
  Body       = cerl:c_cons(cerl:abstract(1), X),
  Let        = cerl:c_let([X], cerl:abstract([2, 3]), Body),
  [1, 2, 3]  = core_eval:expr(Let),

  {comments, ""}.

'fun'(_Config) ->
  X   = cerl:c_var(x),
  Fun = cerl:c_fun([X], X),

  F   = core_eval:expr(Fun),

  1   = F(1),
  foo = F(foo),

  {comments, ""}.

letrec(_Config) ->
  ct:comment("Simple letrec"),
  X      = cerl:c_var(x),
  Fun    = cerl:c_fun([X], X),
  F      = cerl:c_var({f, 1}),
  Body   = cerl:c_apply(F, [cerl:abstract(foo)]),
  Letrec = cerl:c_letrec([{F, Fun}], Body),
  foo    = core_eval:expr(Letrec),

  {comments, ""}.

literal(_Config) ->
  ct:comment("Literal number"),
  1 = core_eval:expr(cerl:abstract(1)),

  ct:comment("More complex literal"),
  NestedMaps = #{foo => #{bar => baz}},
  NestedMaps = core_eval:expr(cerl:abstract(NestedMaps)),

  {comments, ""}.

map(_Config) ->
  X     = cerl:c_var(x),
  Pair  = cerl:c_map_pair(X, cerl:abstract(1)),
  Map   = cerl:c_map([Pair]),
  Let   = cerl:c_let([X], cerl:abstract(one), Map),
  Value = core_eval:expr(Let),
  Value = #{one => 1},

  {comments, ""}.

primop(_Config) ->
  Foo       = cerl:abstract(foo),
  Error     = cerl:abstract(error),

  ct:comment("Raise"),
  Raise     = cerl:abstract(raise),
  RaiseOp   = cerl:c_primop(Raise, [Foo, Error]),
  error     = try core_eval:expr(RaiseOp)
              catch error:foo -> error
              end,

  ct:comment("Match fail"),
  MatchFail   = cerl:abstract(match_fail),
  MatchFailOp = cerl:c_primop(MatchFail, [Foo]),
  error     = try core_eval:expr(MatchFailOp)
              catch error:foo -> error
              end,

  ct:comment("Not implemented"),
  NotImplOp = cerl:c_primop(Foo, [Foo]),
  error     = try core_eval:expr(NotImplOp)
              catch error:{not_implemented, foo} -> error
              end,

  {comments, ""}.

'receive'(_Config) ->
  Pid     = erlang:self(),

  Foo     = cerl:abstract(foo),
  Timeout = cerl:abstract(200),
  Bar     = cerl:abstract(bar),
  Clause  = cerl:c_clause([Foo], Foo),
  Receive = cerl:c_receive([Clause], Timeout, Bar),

  erlang:spawn(fun() -> timer:sleep(100), Pid ! foo end),

  foo     = core_eval:expr(Receive),
  bar     = core_eval:expr(Receive),

  erlang:spawn(fun() -> timer:sleep(100), Pid ! baz end),

  bar     = core_eval:expr(Receive),

  {comments, ""}.

seq(_Config) ->
  Seq = cerl:c_seq(cerl:abstract(foo), cerl:abstract(bar)),
  bar = core_eval:expr(Seq),

  {comments, ""}.

'try'(_Config) ->
  Val  = cerl:abstract(1),
  X    = cerl:c_var(x),
  A    = cerl:c_var(a),
  B    = cerl:c_var(b),
  C    = cerl:c_var(c),

  ct:comment("Return body"),
  Try1 = cerl:c_try(Val, [X], X, [A, B, C], B),
  1    = core_eval:expr(Try1),

  ct:comment("Throw error and catch it"),
  Body = cerl:c_call( cerl:abstract(erlang)
                    , cerl:abstract(error)
                    , [cerl:abstract(foo)]
                    ),
  Try2 = cerl:c_try(Val, [X], Body, [A, B, C], B),
  foo  = core_eval:expr(Try2),

  {comments, ""}.

tuple(_Config) ->
  ct:comment("Simple tuple"),
  X         = cerl:c_var(x),
  Tuple     = cerl:c_tuple([ cerl:abstract(1)
                           , cerl:abstract(2)
                           , cerl:c_var(x)
                           ]),
  Let       = cerl:c_let([X], cerl:abstract(3), Tuple),
  {1, 2, 3} = core_eval:expr(Let),

  {comments, ""}.

values(_Config) ->
  A      = cerl:c_var(a),
  B      = cerl:c_var(b),
  C      = cerl:c_var(c),

  Foo    = cerl:abstract(foo),
  Values = cerl:c_values([Foo, Foo, Foo]),

  Tuple  = cerl:c_tuple([A, B, C]),

  Let    = cerl:c_let([A, B, C], Values, Tuple),
  {foo, foo, foo} = core_eval:expr(Let),

  {comments, ""}.

var(_Config) ->
  X     = cerl:c_var(x),
  {unbound_var, x, _} = try core_eval:expr(X)
                        catch error:Error -> Error
                        end,

  {comments, ""}.

invalid_exprs(_Config) ->
  Foo   = cerl:abstract(foo),
  Catch = cerl:c_catch(Foo),
  {invalid_expr, c_catch} = try core_eval:expr(Catch)
                            catch _:Error1 -> Error1
                            end,

  Clause = cerl:c_clause([Foo], Foo),
  {illegal_expr, _} = try core_eval:expr(Clause)
                      catch _:Error2 -> Error2
                      end,

  Module = cerl:c_module(Foo, [], []),
  {invalid_expr, c_module} = try core_eval:expr(Module)
                             catch _:Error3 -> Error3
                             end,

  {illegal_expr, _} = try core_eval:expr(foo)
                      catch _:Error4 -> Error4
                      end,

  {comments, ""}.

complete_coverage(_Config) ->
  no_expressions = try core_eval:exprs([])
                   catch _:Error4 -> Error4
                   end,

  bar = core_eval:exprs([cerl:abstract(foo), cerl:abstract(bar)]),

  {comments, ""}.
