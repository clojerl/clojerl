# clojerl

[![Travis](https://travis-ci.org/jfacorro/clojerl.svg?branch=master)](https://travis-ci.org/jfacorro/clojerl)

Clojure implemented on the Erlang VM.

## Building

    git clone https://github.com/jfacorro/clojerl
    make

## Rationale

Erlang is a great language for building safe, reliable and scalable
systems. It provides immutable, persistent data structures
out of the box and its concurrency semantics are unequalled by any
other language.

Clojure is a Lisp and as such comes with all the goodies Lisps provide,
Apart from these Clojure also introduces powerful abstractions such as
protocols, multimethods and seqs, to name a few.

Clojure was built to simplify the development of concurrent programs
and some of its concurrency abstractions could be adapted to Erlang.
It is fair to say that combining the power of the Erlang VM with the
expressiveness of Clojure could provide an interesting, useful result
to make the lives of many programmers simpler and make the world a
happier place.

## Goals

- Interoperability as smooth as possible, just like Clojure proper and
  ClojureScript do.
- Provide most Clojure abstractions.
- Provide all Erlang abstractions and toolset.
- Include a default OTP library in Clojerl.

## Personal Goal

Learn more about Erlang (and its VM), Clojure and language implementation.

This project is an experiment that I hope others will find useful.
Regardless of whether it becomes a fully functional implementation of
Clojure or not, I think I will have learned a lot along the way.

## Discussion

You can find me on [twitter](https://twitter.com/jfacorro) or lurking
on [Clojure](https://groups.google.com/forum/?hl=en#!forum/clojure)'s
and
[Erlang](https://groups.google.com/forum/?hl=en#!forum/erlang-programming)'s
mailing lists.

Any feedback, comment and/or suggestion is welcome!
