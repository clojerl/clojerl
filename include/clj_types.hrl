-type namespace() :: atom().
-type name()      :: atom().
-type keyword()   :: {keyword,
                      #{ns => namespace(),
                        name => name()}}.
-type symbol()    :: {symbol,
                      #{ns => namespace(),
                        name => name()}}.

-type 'list*'()   :: {list, list()}.
-type vector()    :: {vector, array:array()}.
-type 'map*'()    :: {map, map()}.
-type set()       :: {set, gb_sets:set()}.


-type sexpr()     :: number() | string() | keyword() | symbol().

-type meta()      :: map().

%% Unsupported / Unused

-type ratio()     :: #{type => ratio,
                       denom => integer(),
                       enum => integer()}.
