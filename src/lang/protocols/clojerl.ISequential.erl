-module('clojerl.ISequential').

-callback noop(_ :: type()) -> any().

-type type() :: any().
