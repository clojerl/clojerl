-module('clojerl.Var').

-export([
         new/2,
         namespace/1,
         name/1,
         dynamic/1, dynamic/2,
         is_macro/1
        ]).

-type info() :: #{ns => binary() | undefined,
                  name => binary(),
                  root => any() | undefined,
                  meta => map(),
                  is_dynamic => boolean(),
                  is_macro => boolean()}.
-type type() :: {?MODULE, info()}.

-spec new('clojerl.Symbol':type(), 'clojerl.Symbol':type()) -> type().
new(NsSym, NameSym) ->
  {?MODULE,
   #{ns => NsSym,
     name => NameSym,
     root => undefined,
     meta => #{},
     is_dynamic => false,
     is_macro => false}}.

-spec namespace(type()) -> 'clojerl.Symbol':type().
namespace({?MODULE, #{ns := Namespace}}) -> Namespace.

-spec name(type()) -> 'clojerl.Symbol':type().
name({?MODULE, #{name := Name}}) -> Name.

-spec dynamic(type()) -> boolean().
dynamic({?MODULE, #{is_dynamic := IsDynamic}}) -> IsDynamic.

-spec dynamic(type(), boolean()) -> type().
dynamic({?MODULE, Data}, IsDynamic) ->
  {?MODULE, Data#{is_dynamic => IsDynamic}}.

-spec is_macro(type()) -> boolean().
is_macro({?MODULE, #{is_macro := IsMacro}}) -> IsMacro.
