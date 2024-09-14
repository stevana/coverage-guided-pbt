# Why is coverage-guided property-based testing still not a thing?

*Work in progress, please don't share, but do feel free to get involved!*

Almost ten years ago, back in 2015, Dan Luu wrote a
[post](https://danluu.com/testing/) asking why coverage-guided property-based
testing wasn't a thing. 

In this post I'll show how one can implement such a thing starting from
scratch. 

The technique is programming language agnostic and doesn't rely on any
language-specific instrumentation of the software under test.

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

* [Crowbar](https://github.com/stedolan/crowbar)
* [FuzzChick](https://dl.acm.org/doi/10.1145/3360607)?

* Hypothesis 
  - Has notion of coverage: https://hypothesis.readthedocs.io/en/latest/details.html#hypothesis.event) 
  - But coverage-guided testing was [removed](https://github.com/HypothesisWorks/hypothesis/pull/1564/commits/dcbea9148be3446392bc3af8892d49f3cc74fbe3) 

* Shae "shapr" Erisson's post [*Run property tests until coverage stops
  increasing*](https://shapr.github.io/posts/2023-07-30-goldilocks-property-tests.html)
  (2023) and [trynocular](https://github.com/shapr/trynocular) library.

## Examples and the main idea of coverage-guidance

To get a better understanding of why coverage-guidence is needed, let's
consider a concrete example.

This example is due to Dmitry Vyukov, the main author of
[go-fuzz](https://github.com/dvyukov/go-fuzz), but it's basically an easier to
understand version of the example from Dan's post[^1].

```
func sut(input []byte) {
    slice := []int{}
    if input[0] == 'A' {
        if input[1] == 'B' {
            if input[2] == 'C' {
                if input[3] == 'D' {
                    slice[input[4]] = 1 // out-of-bounds here
                }
            }
        }
    }
}
```

Even if we generate random inputs of exactly the length 4, it would still take
$O(2^8 * 2^8 * 2^8 * 2^8) = O(2^8^4) = O(2^32) = 4294967296$ tries to trigger
the bug (and obviously even longer if we tried arrays of varying length).

With coverage-guidance we keep track of inputs that resulted in increased
coverage. So, for example, if we generate the array `[]byte{'A'}` we get
further into the nested ifs, and so we take note of that and start generating
longer arrays that start with 'A' and see if we get even further, etc.

By building on previous succeses in getting more coverage, we can effectively
reduce the problem to only need $O(2^8 + 2^8 + 2^8 + 2^8) = O(2^8*4) = O(2^10)
= 1024$ tries. With other words coverage-guidence turns an exponential problem
into a polynomial problem!


* Where do we get the covergage information from?
  - compiler?
  - line/branch?
  - slow, requires compiler with special falgs (in Haskell), different setup in different languages
* Antithesis' ["sometime
  assertions"](https://antithesis.com/docs/best_practices/sometimes_assertions.html)
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


[^1]: Here's Dan's example in full:
  ```
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
  ```
  As I hope we can agree, it's very similar to Dmitry's example, except it's a
  bit less clear what exactly happens in the if statement.


