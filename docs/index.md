## Clojure for the Erlang VM

Erlang is a great language for building safe, reliable and scalable systems. It
provides immutable, persistent data structures out of the box and its
concurrency semantics are unequalled by any other language.

Clojure is a Lisp and as such comes with all the goodies Lisps provide. Apart
from these Clojure also introduces powerful abstractions such as protocols,
multi-methods and seqs, to name a few.

Clojure was originally built to simplify the development of concurrent programs
in the JVM. Some of its concurrency abstractions could be adapted to Erlang, but
this is not the main goal of this project. Its main goal is to combine the power
of the Erlang VM with the expressiveness of Clojure to provide an interesting,
useful result that might make the lives of many programmers simpler and make the
world a happier place.

## Getting Started

Build `clojerl` form source (requires *Erlang/OTP 18+* and *rebar3*):

    git clone https://github.com/jfacorro/clojerl
    cd clojerl
    make

Running `make repl` will start the REPL and show its prompt:

    Clojure 0.0.0-974.592ad8a
    clje.user=>

Or alternatively you can use the `bin/clojerl` command for the same purpose:

    bin/clojerl --clojure.main -r
