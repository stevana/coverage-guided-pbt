# Why is coverage-guided property-based testing still not a thing?

*Work in progress, please don't share, but do feel free to get
involved!*

Almost ten years ago, back in 2015, Dan Luu wrote a
[post](https://danluu.com/testing/) asking why coverage-guided
property-based testing wasn't a thing.

In this post I'll show how one can implement such a thing starting from
scratch.

The technique is programming language agnostic and doesn't rely on any
language-specific instrumentation of the software under test.

## Background and prior work

- AFL

Coverage-guided fuzzers, such as [American Fuzzy
Lop](https://lcamtuf.coredump.cx/afl/) (AFL), have been very successful
in finding [bugs](https://lcamtuf.coredump.cx/afl/#bugs) in programs
that take bytes as input. That means any kind of programs that takes
user strings, command line arguments or files as inputs, parsers, but
also arrays of ints, etc.

- PBT

- Go-fuzz?

- [Crowbar](https://github.com/stedolan/crowbar)

- [FuzzChick](https://dl.acm.org/doi/10.1145/3360607)?

- Hypothesis

  - Has notion of coverage:
    <https://hypothesis.readthedocs.io/en/latest/details.html#hypothesis.event>)
  - But coverage-guided testing was
    [removed](https://github.com/HypothesisWorks/hypothesis/pull/1564/commits/dcbea9148be3446392bc3af8892d49f3cc74fbe3)

## Examples and the main idea of coverage-guidance

Dan Luu's example:

    // Checks that a number has its bottom bits set
    func some_filter(x int) bool {
        for i := 0; i < 16; i = i + 1 {
            if !(x&1 == 1) {
                return false
            }
            x >>= 1
        }
        return true
    }

    // Takes an array and returns a non-zero int
    func dut(a []int) int {
        if len(a) != 4 {
            return 1
        }

        if some_filter(a[0]) {
            if some_filter(a[1]) {
                if some_filter(a[2]) {
                    if some_filter(a[3]) {
                        return 0 // A bug! We failed to return non-zero!
                    }
                    return 2
                }
                return 3
            }
            return 4
        }
        return 5
    }

Dmitry Vyukov, the main author of
[go-fuzz](https://github.com/dvyukov/go-fuzz), gives the follow similar
in spirit but easier to understand example:


    The following code wants "ABCD" input:

        if input[0] == 'A' {
            if input[1] == 'B' {
                if input[2] == 'C' {
                    if input[3] == 'D' {
                        slice[input[4]] = 1  // out-of-bounds here
        }}}}

    Blind generation needs O(2^8^4) = O(2^32) tries.

    Corpus progression:

        0. {}
        1. {"A"}
        2. {"A", "AB"}
        3. {"A", "AB", "ABC"}
        4. {"A", "AB", "ABC", "ABCD"}

    Coverage-guided fuzzer needs O(4 * 2^8) = O(2^10) tries.

- Where do we get the covergage information from?
  - compiler?
  - line/branch?
  - slow, requires compiler with special falgs (in Haskell), different
    setup in different languages
- Antithesis' ["sometime
  assertions"](https://antithesis.com/docs/best_practices/sometimes_assertions.html)
  - generalised coverage
  - not all coverage is the same
- PBT already has notion of coverage built-in ("labels"), which is
  [crucial](https://www.youtube.com/watch?v=NcJOiQlzlXQ) for writing
  good PBTs, can we not just reuse that?

## Prototype implementation

## Testing some examples with the prototype

## Compare with other libraries

- Go-fuzz
- Hypothesis

## Conclusion and further work

- More realistic example, e.g.: leader election, transaction rollback,
  failover?

- Annoying to sprinkle sometimes assertions everywhere?

  - Can it be combined with logging or tracing?

- Use size parameter to implement AFL heuristic for choosing integers?
  Or just use `frequency`?

## See also

- [Coverage guided, property based
  testing](https://dl.acm.org/doi/10.1145/3360607) by Pierce et al
  (2019)

- [Building on developers' intuitions to create effective property-based
  tests](https://www.youtube.com/watch?v=NcJOiQlzlXQ) by John Hughes
  (Lambda Days, 2019)
