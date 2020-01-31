-type config() :: list().
-type result() :: {comments, string()}.

-define(assertEquiv(X, Y)
       , case clj_rt:equiv(X, Y) of
           true -> ok;
           false -> ?ERROR([X, <<" is not equivalent to ">>, Y])
         end
       ).
