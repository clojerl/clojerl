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
        , 'catch'/1
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
  ct:comment("UTF"),
  CasesUtf = #{ {$a, 8, 1, utf8,  [big]}    => <<"a">>
              , {$a, 8, 1, utf8,  [little]} => <<"a">>
              , {$a, 8, 1, utf16, [big]}    => <<0, 97>>
              , {$a, 8, 1, utf16, [little]} => <<97, 0>>
              , {$a, 8, 1, utf32, [big]}    => <<0, 0, 0, 97>>
              , {$a, 8, 1, utf32, [little]} => <<97, 0, 0, 0>>
              },
  assert_segments(CasesUtf),

  ct:comment("Integer"),
  CasesInt = #{ {1, 1, 1, integer, []}                    => <<1:1>>
              , {256, 16, 1, integer, [little, signed]}   => <<0, 1>>
              , {256, 16, 1, integer, [little, unsigned]} => <<0, 1>>
              , {256, 16, 1, integer, [big, signed]}      => <<1, 0>>
              , {256, 16, 1, integer, [big, unsigned]}    => <<1, 0>>
              , {256, 16, 1, integer, [native, signed]}   => <<0, 1>>
              , {256, 16, 1, integer, [native, unsigned]} => <<0, 1>>
              , {256, 16, 1, integer, [unsigned, native]} => <<0, 1>>
              },
  assert_segments(CasesInt),

  ct:comment("Float"),
  CasesFloat = #{ {1.0, 64, 1, float, [little]} => <<0, 0, 0, 0, 0, 0, 240, 63>>
                , {1.0, 64, 1, float, [native]} => <<0, 0, 0, 0, 0, 0, 240, 63>>
                , {1.0, 64, 1, float, [big]}    => <<63, 240, 0, 0, 0, 0, 0, 0>>
                },
  assert_segments(CasesFloat),

  ct:comment("Binary"),
  CasesBinary = #{ {<<"hello">>, all, 1, binary, []} => <<"hello">>
                 , {<<"hello">>, 16, 1, binary, []}  => <<"he">>
                 },
  assert_segments(CasesBinary),

  ok = try assert_segments(#{{<<1:1>>, all, 2, binary, []}  => <<"">>})
       catch _:badarg -> ok
       end,

  {comments, ""}.

call(_Config) ->
  Args  = [cerl:abstract(1), cerl:abstract(1)],
  Plus  = cerl:c_call(cerl:abstract(erlang), cerl:abstract('+'), Args),
  2     = core_eval:expr(Plus),

  {comments, ""}.

'case'(_Config) ->
  VarX     = cerl:c_var(x),
  VarY     = cerl:c_var(y),
  VarXs    = cerl:c_var(xs),
  One      = cerl:abstract(1),
  Two      = cerl:abstract(2),

  ct:comment("Pattern match values"),
  Arg0     = cerl:c_values([One, Two]),
  Clause0  = cerl:c_clause([One, VarX], VarX),
  Case0    = cerl:c_case(Arg0, [Clause0]),
  2        = core_eval:expr(Case0),

  ClauseError0 = cerl:c_clause([One, One], One),
  CaseError0   = cerl:c_case(Arg0, [ClauseError0]),
  try core_eval:expr(CaseError0)
  catch _:{case_clause, _} -> ok
  end,

  ct:comment("Pattern match a list"),
  Arg1     = cerl:abstract([1, 2, 3]),
  Pattern1 = cerl:c_cons(VarX, cerl:c_cons(VarY, VarXs)),
  Clause1  = cerl:c_clause([Pattern1], VarX),
  Case1    = cerl:c_case(Arg1, [Clause1]),
  1        = core_eval:expr(Case1),

  CaseError1 = cerl:c_case(cerl:abstract([1]), [Clause1]),
  try core_eval:expr(CaseError1)
  catch _:{case_clause, _} -> ok
  end,

  ct:comment("Pattern match a map"),
  Arg2     = cerl:abstract(#{foo => bar, baz => foo}),
  MapPair2 = cerl:c_map_pair_exact(cerl:abstract(foo), VarX),
  Pattern2 = cerl:c_map([MapPair2]),
  Clause2  = cerl:c_clause([Pattern2], VarX),
  Clause2False = cerl:c_clause([Pattern2], cerl:abstract(false), VarX),
  Case2    = cerl:c_case(Arg2, [Clause2False, Clause2]),
  bar      = core_eval:expr(Case2),

  ct:comment("Pattern match a literal map"),
  MapPair3 = cerl:c_map_pair_exact(cerl:abstract(foo), cerl:abstract(bar)),
  Pattern3 = cerl:c_alias(VarX, cerl:c_map([MapPair3])),
  Clause3  = cerl:c_clause([Pattern3], VarX),
  Case3    = cerl:c_case(Arg2, [Clause3]),
  Result3  = core_eval:expr(Case3),
  Result3  = cerl:concrete(Arg2),

  CaseError2 = cerl:c_case(cerl:abstract(#{baz => 1}), [Clause2]),
  try core_eval:expr(CaseError2)
  catch _:{case_clause, _} -> ok
  end,

  CaseError3 = cerl:c_case(cerl:abstract(1), [Clause2]),
  try core_eval:expr(CaseError3)
  catch _:{case_clause, _} -> ok
  end,

  ct:comment("Pattern match a tuple"),
  Arg4     = cerl:abstract({foo, bar}),
  Pattern4 = cerl:c_tuple([VarX, VarY]),
  Clause4  = cerl:c_clause([Pattern4], VarY),
  Case4    = cerl:c_case(Arg4, [Clause4]),
  bar      = core_eval:expr(Case4),

  CaseError4a = cerl:c_case(cerl:abstract({foo}), [Clause4]),
  try core_eval:expr(CaseError4a)
  catch _:{case_clause, _} -> ok
  end,

  CaseError4b = cerl:c_case(cerl:abstract(1), [Clause4]),
  try core_eval:expr(CaseError4b)
  catch _:{case_clause, _} -> ok
  end,

  ct:comment("Pattern match a binary"),
  Binary      = <<"foobarbaz">>,
  Arg5        = cerl:abstract(Binary),
  Rest        = {VarY, all, 8, binary,  []},
  CasesBinary = #{ [{VarX, all, 8, binary, []}] => Binary
                 , [{VarX, 3, 8, binary, []}, Rest] => <<"foo">>
                 , [{VarX, 8, 1, integer, [signed, big]}, Rest] => $f
                 , [{VarX, 8, 1, integer, [unsigned, big]}, Rest] => $f
                 , [{VarX, 8, 1, integer, [signed, little]}, Rest] => $f
                 , [{VarX, 8, 1, integer, [unsigned, little]}, Rest] => $f
                 , [{VarX, 8, 1, integer, [signed, native]}, Rest] => $f
                 , [{VarX, 8, 1, integer, [unsigned, native]}, Rest] => $f
                 , [ {VarX, 64, 1, float, [big]}
                   , Rest
                   ] => 2.6714197151370644e185
                 , [ {VarX, 64, 1, float, [little]}
                   , Rest
                   ] => 1.2967274390729776e161
                 , [ {VarX, 64, 1, float, [native]}
                   , Rest
                   ] => 1.2967274390729776e161
                 , [{VarX, undefined, 1, utf8, []}, Rest] => $f
                 , [{VarX, undefined, 1, utf16, [big]}, Rest] => 26223
                 , [{VarX, undefined, 1, utf16, [little]}, Rest] => 28518
                 },

  [ assert_segments_pattern(Arg5, Segments, Result, VarX)
    || {Segments, Result} <- maps:to_list(CasesBinary)
  ],

  ct:comment("UTF32 binaries"),
  ArgUTF32Big = cerl:abstract(<<"foo"/big-utf32>>),
  SegmentsUTF32Big = [{VarX, undefined, 1, utf32, [big]}, Rest],
  assert_segments_pattern(ArgUTF32Big, SegmentsUTF32Big, 102, VarX),

  ArgUTF32Little = cerl:abstract(<<"foo"/little-utf32>>),
  SegmentsUTF32Little = [{VarX, undefined, 1, utf32, [little]}, Rest],
  assert_segments_pattern( ArgUTF32Little, SegmentsUTF32Little, 102, VarX),

  ct:comment("Don't match binaries"),
  Pattern6 = cerl:c_binary([segment(VarX, 2, 8, binary, [])]),
  Clause6  = cerl:c_clause([Pattern6], cerl:abstract(1)),
  CaseError5 = cerl:c_case(cerl:abstract(1), [Clause6]),
  try core_eval:expr(CaseError5)
  catch _:{case_clause, _} -> ok
  end,

  CaseError6 = cerl:c_case(cerl:abstract(<<"foo">>), [Clause6]),
  try core_eval:expr(CaseError6)
  catch _:{case_clause, _} -> ok
  end,

  CaseError7 = cerl:c_case(cerl:abstract(<<"f">>), [Clause6]),
  try core_eval:expr(CaseError7)
  catch _:{case_clause, _} -> ok
  end,

  {comments, ""}.

'catch'(_Config) ->
  ct:comment("Successful catch"),
  Foo   = cerl:abstract(foo),
  Catch = cerl:c_catch(Foo),
  foo   = core_eval:expr(Catch),

  ct:comment("catch with throw"),
  ThrowFoo = cerl:c_call( cerl:abstract(erlang)
                        , cerl:abstract(throw)
                        , [cerl:abstract(foo)]
                        ),
  Catch2   = cerl:c_catch(ThrowFoo),
  foo      = core_eval:expr(Catch2),

  ct:comment("catch with exit"),
  ExitFoo  = cerl:c_call( cerl:abstract(erlang)
                        , cerl:abstract(exit)
                        , [cerl:abstract(foo)]
                        ),
  Catch3   = cerl:c_catch(ExitFoo),
  {'EXIT', foo} = core_eval:expr(Catch3),

  ct:comment("catch with error"),
  ErrorFoo = cerl:c_call( cerl:abstract(erlang)
                        , cerl:abstract(error)
                        , [cerl:abstract(foo)]
                        ),
  Catch4   = cerl:c_catch(ErrorFoo),
  {'EXIT', {foo, _}} = core_eval:expr(Catch4),

  {comments, ""}.

cons(_Config) ->
  ct:comment("Simple cons"),
  X          = cerl:c_var(x),
  Body       = cerl:c_cons(cerl:abstract(1), X),
  Let        = cerl:c_let([X], cerl:abstract([2, 3]), Body),
  [1, 2, 3]  = core_eval:expr(Let),

  {comments, ""}.

'fun'(_Config) ->
  [ begin
      ct:comment("fun with ~p args", [N]),
      Fun = fun_of_arity(N),
      F   = core_eval:expr(Fun),
      N   = apply(F, lists:seq(1, N))
    end
    || N <- lists:seq(1, 20)
  ],

  ct:comment("fun with 0 args"),
  Fun = fun_of_arity(0),
  F   = core_eval:expr(Fun),
  0   = apply(F, []),

  {comments, ""}.

letrec(_Config) ->
  [ begin
      ct:comment("letrec with ~p args", [N]),
      Fun    = fun_of_arity(N),
      F      = cerl:c_var({f, N}),
      Args   = [cerl:abstract(X) || X <- lists:seq(1, N)],
      Body   = cerl:c_apply(F, Args),
      Letrec = cerl:c_letrec([{F, Fun}], Body),
      N      = core_eval:expr(Letrec)
    end
    || N <- lists:seq(1, 20)
  ],

  ct:comment("letrec with 0 args"),
  Fun    = fun_of_arity(0),
  F      = cerl:c_var({f, 0}),
  Body   = cerl:c_apply(F, []),
  Letrec = cerl:c_letrec([{F, Fun}], Body),
  0      = core_eval:expr(Letrec),

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
  Foo = cerl:abstract(foo),

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


%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

segment({Val, Size, Unit, Type, Flags}) ->
  segment(Val, Size, Unit, Type, Flags).

segment(Val0, Size, Unit, Type, Flags) ->
  Val1 = case cerl:is_c_var(Val0) of
           true -> Val0;
           false -> cerl:abstract(Val0)
         end,
  cerl:c_bitstr( Val1
               , cerl:abstract(Size)
               , cerl:abstract(Unit)
               , cerl:abstract(Type)
               , cerl:abstract(Flags)
               ).

assert_segments(Cases) ->
  [ begin
      ct:comment("~p => ~p", [Segment, Result]),
      {Val, Size, Unit, Type, Flags} = Segment,
      SegmentInt   = segment(Val, Size, Unit, Type, Flags),
      BitStringInt = cerl:c_binary([SegmentInt]),
      Result       = core_eval:expr(BitStringInt)
    end
    || {Segment, Result} <- maps:to_list(Cases)
  ].

assert_segments_pattern(Arg, SegmentsSpec, Result, Var) ->
  ct:comment("Binary pattern: ~p -> ~p", [SegmentsSpec, Result]),
  Segments = lists:map(fun segment/1, SegmentsSpec),
  Pattern  = cerl:c_binary(Segments),
  Clause   = cerl:c_clause([Pattern], Var),
  Case     = cerl:c_case(Arg, [Clause]),
  Result   = core_eval:expr(Case).

fun_of_arity(0) ->
  cerl:c_fun([], cerl:abstract(0));
fun_of_arity(N) ->
  Vars = [cerl:c_var(X) || X <- lists:seq(1, N)],
  cerl:c_fun(Vars, lists:last(Vars)).
