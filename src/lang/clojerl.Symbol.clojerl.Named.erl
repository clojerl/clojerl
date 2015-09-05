-module('clojerl.Symbol.clojerl.Named').
-behavior('clojerl.Named').

-export([name/1, namespace/1]).

-spec name('clojerl.Symbol':type()) -> binary().
name({_, #{name := Name}}) ->
  Name.

-spec namespace('clojerl.Symbol':type()) -> binary().
namespace({_, #{ns := Namespace}}) ->
  Namespace.
