# Coverage-guided property-based testing

*Work in progress, please don't share, but do feel free to get involved!*

Almost ten years ago, back in 2015, Dan Luu wrote a
[post](https://danluu.com/testing/) asking why coverage-guided property-based
testing wasn't a thing. In this post I'll show how one can implement such a
thing starting from scratch. I'll be using Haskell, but the technique is
programming language agnostic.

## Background and prior work

* AFL

Coverage-guided fuzzers, such as [American Fuzzy
Lop](https://lcamtuf.coredump.cx/afl/) (AFL), have been very successful in
finding [bugs](https://lcamtuf.coredump.cx/afl/#bugs) in programs that take
bytes as input. That means any kind of programs that takes user strings,
command line arguments or files as inputs, parsers, but also arrays of ints,
etc.

* PBT

* Go-fuzz?
* Hypothesis?
* [fuzzChick](https://dl.acm.org/doi/10.1145/3360607)?

## Examples and the main idea of coverage-guidance

* Where do we get th ecovergage information from?
  - compiler?
  - line/branch?
  - slow, requires compiler with special falgs (in Haskell), different setup in different languages
* Antithesis' "sometime assertions" 
  - generalised coverage
  - not all coverage is the same
* PBT already has notion of coverage built-in ("labels"), which is
  [crucial](https://www.youtube.com/watch?v=NcJOiQlzlXQ) for writing good PBTs,
  can we not just reuse that?

## Prototype implementation

## Testing some examples with the prototype

## Compare with other libraries

* Go-fuzz
* Hypothesis

## Conclusion and further work

* More realistic example, e.g.: leader election, transaction rollback,
  failover?
* Annoying to sprinkle sometimes assertions everywhere?
  - Can it be combined with logging or tracing?

* Use size parameter to implement AFL heuristic for choosing integers? Or just
  use `frequency`?

## See also

* [Coverage guided, property based
  testing](https://dl.acm.org/doi/10.1145/3360607) by Pierce et al (2019)

* [Building on developers' intuitions to create effective property-based
  tests](https://www.youtube.com/watch?v=NcJOiQlzlXQ) by John Hughes (Lambda
  Days, 2019)
