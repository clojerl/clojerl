-module('clojerl.ISequential').

-callback 'clojerl.ISequential.noop'(_ :: type()) -> any().

-type type() :: any().
