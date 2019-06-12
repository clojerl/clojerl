-ifndef(CLOJERL_PROTOCOL).

-define(CLOJERL_PROTOCOL, true).

-define(CATCH_ALL_TAG, '$catch_all$').
-define(CATCH_ALL_MAP_TAG, '$catch_all_map$').

-define( PRIMITIVE_TYPES
       , #{ 'clojerl.String'    => #{pred => is_binary,    order => 1}
          , 'clojerl.BitString' => #{pred => is_bitstring, order => 2}
          , 'clojerl.Integer'   => #{pred => is_integer,   order => 3}
          , 'clojerl.Float'     => #{pred => is_float,     order => 4}
          , 'clojerl.Boolean'   => #{pred => is_boolean,   order => 5}
          , 'erlang.List'       => #{pred => is_list,      order => 6}
          , 'erlang.Map'        => #{pred => is_map,       order => 7}
          , 'erlang.Tuple'      => #{pred => is_tuple,     order => 8}
          , 'erlang.Fn'         => #{pred => is_function,  order => 9}
          , ?NIL_TYPE           => #{pred => ?NIL,         order => 10}
          , 'clojerl.Keyword'   => #{pred => is_atom,      order => 11}
          , 'erlang.Port'       => #{pred => is_port,      order => 12}
          , 'erlang.Process'    => #{pred => is_pid,       order => 13}
          , 'erlang.Reference'  => #{pred => is_reference, order => 14}
          }
       ).

-define( GUARDS2TYPES
       , #{ is_binary    => 'clojerl.String'
          , is_bitstring => 'clojerl.BitString'
          , is_integer   => 'clojerl.Integer'
          , is_float     => 'clojerl.Float'
          , is_boolean   => 'clojerl.Boolean'
          , is_list      => 'erlang.List'
          , is_map       => 'erlang.Map'
          , is_tuple     => 'erlang.Tuple'
          , is_function  => 'erlang.Fn'
          , is_atom      => 'clojerl.Keyword'
          , is_port      => 'erlang.Port'
          , is_pid       => 'erlang.Process'
          , is_reference => 'erlang.Reference'
          }).

-endif.
