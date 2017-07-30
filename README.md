# clojerl

[![Travis](https://travis-ci.org/jfacorro/clojerl.svg?branch=master)](https://travis-ci.org/jfacorro/clojerl)

Clojure implemented on the Erlang VM.

## Building

Building `clojerl` requires *Erlang/OTP 18+* and *rebar3*.

    git clone https://github.com/jfacorro/clojerl
    cd clojerl
    make

## Getting Started

Running `make repl` will start the REPL and show its prompt:

    Clojure 0.0.0-974.592ad8a
    clje.user=>

From the REPL it's possible to start evaluating Clojure expressions:

    clje.user=> (map inc (range 10))
    (1 2 3 4 5 6 7 8 9 10)
    clje.user=> (doc map)
    -------------------------
    clojure.core/map
    ([f] [f coll] [f c1 c2] [f c1 c2 c3] [f c1 c2 c3 & colls])
      Returns a lazy sequence consisting of the result of applying f to
      the set of first items of each coll, followed by applying f to the
      set of second items in each coll, until any one of the colls is
      exhausted.  Any remaining items in other colls are ignored. Function
      f should accept number-of-colls arguments. Returns a transducer when
      no collection is provided.
    nil
    clje.user=> (doc inc)
    -------------------------
    clojure.core/inc
    ([x])
      Returns a number one greater than num. Does not auto-promote
      longs, will throw on overflow. See also: inc'
    nil
    clje.user=>

For a very basic example of a project please check the [example-web-app](https://github.com/clojerl/example-web-app/) repository.

## Rationale

Erlang is a great language for building safe, reliable and scalable
systems. It provides immutable, persistent data structures
out of the box and its concurrency semantics are unequalled by any
other language.

Clojure is a Lisp and as such comes with all the goodies Lisps
provide. Apart from these Clojure also introduces powerful
abstractions such as protocols, multimethods and seqs, to name a few.

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

### Personal Goal

Learn more about Erlang (and its VM), Clojure and language
implementation.

This project is an experiment that I hope others will find useful.
Regardless of whether it becomes a fully functional implementation of
Clojure or not, I will have learned a lot along the way.

## QAs

### What is Clojerl?

Clojerl is an experimental implementation of Clojure on the Erlang VM.
Its goal is to leverage the features and abstractions of Clojure that
we love (macros, collections, seq, protocols, multimethods, metadata,
etc.), with the robustness the Erlang VM provides for building
(distributed) systems.

### Have you heard about LFE and Joxa?

Yes. LFE and Joxa were each created with very specific and different
goals in mind. LFE was born to provide a LISP syntax for Erlang. Joxa
was mainly created as a platform for creating DSLs that could take
advantage of the Erlang VM. Its syntax was inspired by Clojure but the
creators weren't interested in implementing all of Clojure's features.

### Aren't the language constructs for concurrency very different between Clojure and Erlang?

Yes, they are. On one hand Clojure provides tools to handle mutable
state in a sane way, while making a clear distinction between identity
and state through reference types. On the other, concurrency in the
Erlang VM is implemented through processes and message passing. The
idea in Clojerl is to encourage the Erlang/OTP concurrency model, but
support as many Clojure constructs as possible and as far as they make
sense in the Erlang VM.

### But... but... Rich Hickey lists [here](https://clojure.org/about/state#actors) some of the reasons why he chose not to use the actor model in Clojure.

That is not a question, but I see what you mean :). The points he
makes are of course very good. For example, when no state is shared
between processes there is some communication overhead, but this
isolation is also an advantage under a lot of circumstances. He also
mentions
[here](https://groups.google.com/forum/#!msg/clojure/Kisk_-9dFjE/_2WxSxyd1SoJ) that
building for the distributed case (a.k.a processes and message
passing) is more complex and not always necessary, so he decided to
optimise for the non-distributed case and add distribution to the
parts of the system that need it. Rich Hickey calls Erlang "quite
impressive", so my interpretation of these writings is that they are
more about exposing the rationale behind the decisions and the
trade-offs he made when designing Clojure (on the JVM), than about
disregarding the actor model.

### Will Clojerl support every single Clojure feature?

No. Some of Clojure's features are implemented by relying on the
underlying mutability of the JVM and its object system. The Erlang VM
provides very few mutability constructs and no support for defining
new types. This makes it very hard or nearly impossible to port some
features into Clojerl's implementation.

### Can I reuse existing Clojure(Script) libraries?

Yes, but they will need to be ported, just like for ClojureScript. In
fact, most of Clojure's core namespaces where ported from the original
.clj files in the Clojure JVM repository.

## Discussion

You can find me on [twitter](https://twitter.com/jfacorro) or lurking
on [Clojure](https://groups.google.com/forum/?hl=en#!forum/clojure)'s
and
[Erlang](https://groups.google.com/forum/?hl=en#!forum/erlang-programming)'s
mailing lists.

Any feedback, comment and/or suggestion is welcome!
