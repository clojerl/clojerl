%% Use a value that would be invalid for a keyword literal
-define(TYPE, '7ype').
-define(M, ?MODULE).

-record(?TYPE, { name = ?M  :: atom()
               , data       :: any()
               , info = #{} :: map()
               }).
