-type namespace() :: atom().
-type name()      :: atom().
-type keyword()   :: #{type => keyword,
                       ns => namespace(),
                       name => name(),
                       meta => map()}.
-type symbol()    :: #{type => symbol,
                       ns => namespace(),
                       name => name(),
                       meta => map()}.

-type sexpr() :: number() | string() | keyword() | symbol().
