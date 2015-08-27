-module('clojerl.Keyword.clojerl.Named').
-behavior('clojerl.Named').

-export([name/1, namespace/1]).

-spec name('clojerl.Keyword':type()) -> binary().
name({_, #{name := Name}}) -> Name.

-spec namespace('clojerl.Keyword':type()) -> binary().
namespace({_, #{ns := Namespace}}) -> Namespace.
