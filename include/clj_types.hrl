-type namespace() :: atom().
-type name()      :: atom().
-type keyword()   :: {keyword,
                      #{ns => namespace(),
                        name => name()}}.
-type symbol()    :: {symbol,
                      #{ns => namespace(),
                        name => name()}}.

-type vector()    :: {vector, array:array()}.

-type sexpr()     :: number() | string() | keyword() | symbol().

-type meta()      :: map().

%% Unsupported / Unused

-type ratio()     :: #{type => ratio,
                       denom => integer(),
                       enum => integer()}.
