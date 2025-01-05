# clojerl

![Build](https://github.com/clojerl/clojerl/workflows/Build/badge.svg)
[![Hex.pm](https://img.shields.io/hexpm/v/clojerl.svg)](https://hex.pm/packages/clojerl)

Clojure implemented on the Erlang VM.

## Building

Building `clojerl` requires *Erlang/OTP 21+* and [*rebar3*][rebar3].

    git clone https://github.com/clojerl/clojerl
    cd clojerl
    make

On Windows:

    git clone https://github.com/clojerl/clojerl
    cd clojerl
    rebar3 clojerl compile

## Getting Started

### Documentation and Resources

There is more information regarding Clojerl in [clojerl.io][clojerl], where you
can find what [features][features] does Clojerl include and [how it differs from
Clojure][diff-with-clojure].

### Online REPL

To try it out and get a sense of what you can do, you can visit
[Try Clojerl][try-clojerl].

## Docker REPL

To quickly try out `clojerl` via docker you can make use of the docker
image like so:

```
docker pull clojerl/clojerl
docker run -it clojerl/clojerl
```

Then you should be able to see the prompt:

```clojure
Clojure 0.6.0
clje.user=>
```


### Local REPL

Running `make repl` (on Windows first run `rebar3 clojerl compile` and
then `bin/clje.bat`) will start the REPL and show its prompt:

    Clojure 0.6.0
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
      Returns a number one greater than num.
    nil
    clje.user=>

### Code Examples

There are some very basic examples in the [scripts/examples][examples]
directory. These are meant to be references on how special forms in
Clojure on the BEAM are used and how they sometimes differ from Clojure
JVM.

### Web Application Example

For a very basic example of a web project please check the
[example-web-app][example-web-app] repository.

### Building Your Own App

The build tool for Clojerl is the [`rebar3_clojerl`][rebar3_clojerl]
plugin. [`rebar3`][rebar3] is the official build tool in the Erlang
community.

The plugin provides helpful commands to:

- Build a basic directory scaffolding for a new project
- Compile
- Run tests
- Start a REPL

For more information on how to use this plugin please check the
documentation in [`rebar3_clojerl`][rebar3_clojerl].

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
fact, most of Clojure's core namespaces were ported from the original
.clj files in the Clojure JVM repository.

## Discussion

Join the conversation in the [Clojerl][clojerl-mailing-list] mailing
list or in the [`#clojerl` Slack channel][clojerl-slack]!

You can also find news and updates through [@clojerl][clojerl-twitter].
Or if you have any questions you can find me [@jfacorro][jfacorro-twitter] or lurking
on [Clojure](https://groups.google.com/forum/?hl=en#!forum/clojure)'s
and
[Erlang](https://groups.google.com/forum/?hl=en#!forum/erlang-programming)'s
mailing lists.

Any feedback, comment and/or suggestion is welcome!

[rebar3]: https://github.com/erlang/rebar3
[try-clojerl]: http://try.clojerl.io/
[examples]: scripts/examples
[example-web-app]: https://github.com/clojerl/example-web-app/
[rebar3_clojerl]:https://github.com/clojerl/rebar3_clojerl
[clojerl]: http://clojerl.io/
[features]: http://clojerl.io/available-features
[diff-with-clojure]: http://clojerl.io/differences-with-clojure
[clojerl-mailing-list]: https://groups.google.com/forum/#!forum/clojerl
[clojerl-slack]: https://erlanger.slack.com
[clojerl-twitter]: https://twitter.com/clojerl
[jfacorro-twitter]: https://twitter.com/jfacorro
