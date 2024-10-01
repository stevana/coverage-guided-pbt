# Coverage-guided property-based testing

*Work in progress, please don't share, but do feel free to get
involved!*

Almost ten years ago, back in 2015, Dan Luu wrote a
[post](https://danluu.com/testing/) asking why coverage-guided
property-based testing wasn't a thing.

In this post I'll survey the coverage-guided landscape, looking at what
was there before Dan's post and what has happened since.

The short version is: today imperative languages seem to be in the
forefront of combining coverage-guidance and property-based testing.

In an effort to try to help functional programming languages catch up,
I'll show how coverage-guidence can be added to the first version of the
original property-based testing tool, QuickCheck, in about 35 lines of
code.

The technique is programming language agnostic and doesn't rely on any
language-specific instrumentation of the software under test (unlike
previous implementations of this idea).

## Motivation

Before we start, let me try to motivate why one would want to combine
coverage-guided fuzzing and property-based testing to begin with.

Consider the following example[^1], where an error is triggered if some
input byte array starts with the bytes `"bad!"`:

    func sut(input []byte) {
        if input[0] == 'b' {
            if input[1] == 'a' {
                if input[2] == 'd' {
                    if input[3] == '!' {
                        panic("input must not be bad!")
                    }
                }
            }
        }
    }

If we were to try to test this function with property-based testing,
where we restrict the input to be of exactly length 4, then it would
still take
$\mathcal{O}(2^8 \cdot 2^8 \cdot 2^8 \cdot 2^8) = \mathcal{O}((2^8)^4) =
\mathcal{O}(2^{32}) \approx 4B$ tries to trigger the bug! A more
realistic test wouldn't fix the length of the input, which would make
the probability of triggering the bug even lower.

With coverage-guidance we keep track of inputs that resulted in
increased coverage. So, for example, if we generate the array
`[]byte{'A'}` we get further into the nested ifs, and so we take note of
that and start generating longer arrays that start with 'A' and see if
we get even further, etc.

By building on previous succeses in getting more coverage, we can
effectively reduce the problem to only need
$\mathcal{O}(2^8 + 2^8 + 2^8 + 2^8) =
\mathcal{O}(2^8 \cdot 4) = \mathcal{O}(2^{10}) = 1024$ tries.

In other words coverage-guidence turns an exponential problem into a
polynomial problem!

## Background and prior work

### Before 2015

Fuzzing has an interesting origin. It was a class
[project](http://pages.cs.wisc.edu/~bart/fuzz/CS736-Projects-f1988.pdf)
in an advanced operating systems course taught by Barton Miller at the
University of Wisconsin in 1988.

The project was inspired by the observation that back then, if you
logged into your workstation via a dail-up modem from home and it
rained, then frequently random characters would appear in the terminal.
The line noise wasn't the surprising thing, but rather that the extra
characters would sometimes crash the program that they tried to invoke.

Among these programs were basic utilities such as `vi`, `mail`, `cc`,
`make`, `sed`, `awk`, `sort`, etc, and it was reasonable to expect that
these would give an error message rather than crash and core dump if fed
with some extra characters caused by the rain.

So the project set out to basically recreate what the rain did, but more
effectively, but essentially generating random noise (stream of bytes)
and feeding that to different utilities and see if they crashed.

A couple of years later Barton et al published [*An empirical study of
the reliability of UNIX
utilities*](https://dl.acm.org/doi/10.1145/96267.96279) (1990) which
documents their findings.

Inserting random characters was effective in finding corner cases where
the programmers forgot to properly validate the input from the user.

However it wouldn't trigger bugs hiding deeper under the surface, such
as the "bad!" example from the previous section.

This changed around 2007 when people [started
thinking](https://lcamtuf.coredump.cx/afl/historical_notes.txt) about
how fuzzing can be combined with [evolutionary
algorithms](https://en.wikipedia.org/wiki/Evolutionary_algorithm).

The idea being that instead of generating random bytes all the time as
with classical fuzzing, we can use coverage information from one test to
mutate the input for the next test. Or to use the evolution metaphor:
seeds that lead to better coverage are mutated with the hope that they
will lead to even better coverage.

One of the first, and perhaps still most widely known, such
*coverage-guided* fuzzers is called
[AFL](https://lcamtuf.coredump.cx/afl/) (2013).

To give you an idea of how powerful this idea is, check out the list of
[bugs](https://lcamtuf.coredump.cx/afl/#bugs) that it found and this
post about how it manages to figure out the [jpeg
format](https://lcamtuf.blogspot.com/2014/11/pulling-jpegs-out-of-thin-air.html)
on its own[^2].

Since AFL is the tool that Dan explicitly mentions in his post, let's
stop at this point and go back to his point, before looking at what
happened since with coverage-guided fuzzers.

Recall that Dan was asking why this idea of coverage-guidance wasn't
present in property-based testing tools.

I've written about the
[history](https://stevana.github.io/the_sad_state_of_property-based_testing_libraries.html#the-history-of-property-based-testing)
of property-based testing and explained how it
[works](https://stevana.github.io/the_sad_state_of_property-based_testing_libraries.html#pure-property-based-testing-recap)
already, so I won't take up space by repeating myself here. Let's just
note that the [original
paper](https://www.cs.tufts.edu/~nr/cs257/archive/john-hughes/quick.pdf)
on property-based testing was published in 2000.

So when Dan wrote asking about this question property-based testing
would have been fifteen and AFL two years old.

The main difference between property-based testing and fuzzing is that
fuzzing requires less work by the user. Simply hook up the byte
generator to the function that expects bytes as input and off it goes
looking for crashes.

Property-based testing on the other hand can test functions that take
arbitrary data structures as input (not just bytes), but you have to
describe how to generate such inputs. Fuzzing only looks for crashes,
while property-based testing lets you specify arbitrary relations that
should hold between the input and output of the system under test.

For example, we can generate binary search trees and check that after we
insert something into an arbitrary binary search tree then it will
remain sorted (when we do an inorder traversal). Fuzzing can't check
such properties, and generating random bytes would seldom lead to valid
binary search trees.

On the other hand, the coverage of property-based tests is only as good
as the user provided generators. Corner cases where slightly modified
data leads to e.g. exception handling is not explored automatically, and
coverage information is not used to guide the input generation process.

By now we should have enough background to see that the idea of
combining coverage-guidance and property-based testing makes sense. What
if we can use user provided generators to kick start the exploration,
but then mutate the data and use coverage information to find problems
that wouldn't have been surfaced with the user provided input generators
alone, or recall from the example in the introduction, the probability
of generating the input in a single shot is simply too unlikely.

### After 2015

Having covered what had happened before Dan's post, let's have a look at
what has happened in the ten years since his post.

First off, it's worth noting that at some point he added an update to
his post:

> "Update: Dmitry Vyukov's Go-fuzz, which looks like it was started a
> month after this post was written, uses the approach from the proof of
> concept in this post of combining the sort of logic seen in AFL with a
> QuickCheck-like framework, and has been shown to be quite effective. I
> believe David R. MacIver is also planning to use this approach in the
> next version of hypothesis."

So let's start there, with Go-fuzz. At a first glance, all functions
that are tested with Go-fuzz need to take an array of bytes as input. My
initial thought was, how can I write properties which involve generating
more interesting data structures? But it turns out it's
[possible](https://news.ycombinator.com/item?id=40876822), as somebody
pointed out in the comments of an old post of mine. Furthermore there
are also
[ways](https://adalogics.com/blog/structure-aware-go-fuzzing-complex-types)
of making the fuzzer aware of more complex data structures. I haven't
played around enough with this to be able to compare how well shrinking
works yet though, but overall I'd say this ticks the box of being a
property-based testing tool that is also coverage-guided.

Next up Dan mentions Python's Hypothesis. I was searching through the
documentation trying to find out how coverage-guidance works, but I
couldn't find anything. Searching through the repository I found the
following [release
note](https://github.com/HypothesisWorks/hypothesis/pull/1564/commits/dcbea9148be3446392bc3af8892d49f3cc74fbe3)
(2018):

> "This release deprecates the coverage-guided testing functionality, as
> it has proven brittle and does not really pull its weight.
>
> We intend to replace it with something more useful in the future, but
> the feature in its current form does not seem to be worth the cost of
> using, and whatever replaces it will likely look very different."

As far as I can tell, it hasn't been reintroduced since.

However it's possible to hook Hypothesis up to [use external
fuzzers](https://hypothesis.readthedocs.io/en/latest/details.html#use-with-external-fuzzers).

Hypothesis already uses random bytes as basis for its generators, unlike
QuickCheck which uses an integer seed, so I suppose that the external
fuzzers essentially fuzz the random input bytes that in turn are used to
generate more structured input.

Traditional fuzzers are usually designed to target a single binary,
where as the test suite which uses property-based testing typically has
many properties. The
[HypoFuzz](https://hypofuzz.com/docs/features.html#fuzzer-details) tool
works around this mismatch by scheduling fuzzing time among the many
Hypothesis properties in your test suite.

What else has happenend since Dan's post?

One of the first things I noticed is that AFL is no longer
[maintained](https://lcamtuf.coredump.cx/afl/):

> "Note: AFL hasn't been updated for a couple of years; while it should
> still work fine, a more complex fork with a variety of improvements
> and additional features, known as AFL++, is available from other
> members of the community and is worth checking out."

[AFL++](https://www.usenix.org/system/files/woot20-paper-fioraldi.pdf)
(2020)

- incorporates all of
  [AFLFast](https://mboehme.github.io/paper/CCS16.pdf)'s [power
  schedules](https://aflplus.plus/docs/power_schedules/) and adds some
  new ones
- explain what power schedules are?
- <https://github.com/mboehme/aflfast>

<!-- -->

- When you search for "coverage-guided property-based testing" in the
  academic literature

- [*Coverage guided, property based
  testing*](https://dl.acm.org/doi/10.1145/3360607) by Leonidas
  Lampropoulos, Michael Hicks, Benjamin C. Pierce (2019)

- FuzzChick Coq/Rocq library

- Not released, lives in an [unmaintained
  branch](https://github.com/QuickChick/QuickChick/compare/master...FuzzChick)
  that [doesn't
  compile](https://github.com/QuickChick/QuickChick/issues/277)?

  - coverage info is [same as in
    AFL](https://youtu.be/RR6c_fiMfJQ?t=2226)

- FuzzChick, related work mentions:

- [JQF + Zest: Coverage-guided semantic fuzzing for
  Java](https://github.com/rohanpadhye/jqf)?

- [Crowbar](https://github.com/stedolan/crowbar)

  - [extended abstract from OCaml
    workshop](https://github.com/ocaml/ocaml.org-media/blob/086fc25105cbccb188c28ec74126d72962921ff8/meetings/ocaml/2017/extended-abstract__2017__stephen-dolan_mindy-preston__testing-with-crowbar.pdf)
    (2017)
  - Uses fuzzing indirectly to generate the data?

- [libfuzzer](https://llvm.org/docs/LibFuzzer.html) and it's successor
  [FuzzTest](https://github.com/google/fuzztest) ("It is a
  first-of-its-kind tool that bridges the gap between fuzzing and
  property-based testing") (2022?)

  - Difference to go-fuzz?

- [honggfuzz](https://github.com/google/honggfuzz)

  - open PR to add it to cargo fuzz:
    <https://github.com/rust-fuzz/book/pull/14>

- [Structure-aware fuzzing using libfuzzer-sys in
  Rust](https://rust-fuzz.github.io/book/cargo-fuzz/structure-aware-fuzzing.html)

- [MUTAGEN: Reliable Coverage-Guided, Property-Based Testing using
  Exhaustive
  Mutations](https://www.mista.me/assets/pdf/icst23-preprint.pdf) (2023)

  - <https://github.com/OctopiChalmers/mutagen/>
  - Uses GHC
    [plugin](https://github.com/OctopiChalmers/mutagen/blob/main/src/Test/Mutagen/Tracer/Plugin.hs)
    to annotate source code with coverage information of: function
    clauses, case statements, multi-way ifs, and each branch of
    if-then-else expressions

Imperative languages such as Go, Python, C++, Rust, and Java seem ahead
of functional languages when it comes to combining coverage-guided
fuzzing and property-based testing.

Let's try to change that by implementing a small functional programming
version, based on the original property-based testing implementation.

## Prototype implementation

One key question we need to answer in order to be able to implement
anything that's coverage-guided is: where do we get the coverage
information from?

### Getting the coverage information

AFL and `go-fuzz` both get it from the compiler.

AFL injects code into every [basic
block](https://en.wikipedia.org/wiki/Basic_block).

When I've been thinking about how to implement coverage-guided
property-based testing in the past, I always got stuck thinking that
parsing the coverage output from the compiler in between test case
generation rounds would be annoying and slow.

I didn't know that you could get this information from a library
provided by the GHC compiler in Haskell, until I read Shae "shapr"
Erisson does in his
[post](https://shapr.github.io/posts/2023-07-30-goldilocks-property-tests.html).

- Footnote? Shae "shapr" Erisson's post [*Run property tests until
  coverage stops
  increasing*](https://shapr.github.io/posts/2023-07-30-goldilocks-property-tests.html) (2023)
  and [trynocular](https://github.com/shapr/trynocular) library.
  - This only uses coverage as a stopping condition, not to actually
    drive the generation...

While this certainly makes things easier, it wasn't until I read about
Antithesis' ["sometime
assertions"](https://antithesis.com/docs/best_practices/sometimes_assertions.html)
that I started seeing a really simple solution to the problem.

These "sometimes assertions" can be thought of as generalised coverage,
in that if we would annotate every single line, expression or branch
with a sometime assertion we'd get back line-, expression-, or
branch-based coverage.

But the cool thing about "sometimes assertions" is that we don't need to
annotate every single line, expression or branch, we can annotate
*interesting* points in our program.

The final piece of the puzzle, and I think this is the only original
idea that this post adds[^3], is that property-based testing already has
functionality for implementing "sometimes assertions": the `label`,
`classify` and `collect` machinary for gathering run-time statistics of
the generated data!

This machinary is [crucial](https://www.youtube.com/watch?v=NcJOiQlzlXQ)
for writing good tests and has been part of the QuickCheck
implementation since the very first version[^4]!

So the question is: can we implement coverage-guided property-based
testing using the internal notion of coverage that property-based
testing already has?

### The first version of QuickCheck

For the sake of self-containment, let's reproduce the the essential
parts of QuickCheck as defined in the appendix of the original
[paper](https://dl.acm.org/doi/10.1145/351240.351266) (ICFP, 2000).

#### Generating input data

Let's start with the generator[^5]:

``` haskell
newtype Gen a = Gen (Int -> StdGen -> a)

generate :: Int -> StdGen -> Gen a -> a
generate n rnd (Gen m) = m size rnd'
 where
  (size, rnd') = randomR (0, n) rnd
```

So a `Gen a` is basically a function from a size and a pseudo-random
number generator into `a`. The pseudo-random number generator and size
can be accessed using the following two functions:

``` haskell
rand :: Gen StdGen
rand = Gen (\_n r -> r)

sized :: (Int -> Gen a) -> Gen a
sized fgen = Gen (\n r -> let Gen m = fgen n in m n r)
```

Using these together with the `Functor`, `Applicative` and `Monad`
instances of `Gen` we can derive other useful combinators for generating
data:

``` haskell
choose :: Random a => (a, a) -> Gen a
choose bounds = (fst . randomR bounds) `fmap` rand

elements :: [a] -> Gen a
elements xs = (xs !!) `fmap` choose (0, length xs - 1)

vector :: Arbitrary a => Int -> Gen [a]
vector n = sequence [ arbitrary | _i <- [1..n] ]
```

Instead of defining generators directly for different datatypes,
QuickCheck first wraps generators in a type class called
`Arbitrary`[^6]:

``` haskell
class Arbitrary a where
  arbitrary :: Gen a

instance Arbitrary Bool where
  arbitrary = elements [True, False]

instance Arbitrary Char where
  -- Avoids generating control characters.
  arbitrary = choose (32,126) >>= \n -> return (chr n)

instance Arbitrary Int where
  arbitrary = sized $ \n -> choose (-n,n)

instance Arbitrary a => Arbitrary [a] where
  arbitrary = sized (\n -> choose (0,n) >>= vector)

```

#### Specifying properties

Next up, let's look at how properties are expressed. The `Property` type
is a wrapper around `Gen Result`:

``` haskell
newtype Property
  = Prop (Gen Result)

result :: Result -> Property
result res = Prop (return res)
```

Where `Result` is defined as follows:

``` haskell
data Result
  = Result { ok :: Maybe Bool, stamp :: [String], arguments :: [String] }

nothing :: Result
nothing = Result{ ok = Nothing, stamp = [], arguments = [] }
```

The idea being that `ok :: Maybe Bool` is `Nothing` if the input gets
discarded and otherwise the boolean indicates whether the property
passed or not. The `stamp` field is used to collect statistics about the
generated test cases, while `arguments` contains all the generated
inputs (or arguments) to the property.

In order to allow the user to write properties of variying arity another
type class is introduced:

``` haskell
class Testable a where
  property :: a -> Property

instance Testable () where
  property _ = result nothing

instance Testable Bool where
  property b = result (nothing{ ok = Just b })

instance Testable Result where
  property res = result res

instance Testable Property where
  property prop = prop

instance (Arbitrary a, Show a, Testable b) => Testable (a -> b) where
  property f = forAll arbitrary f
```

The key ingredient in the function instance of `Testable` is `forAll`:

``` haskell
forAll :: (Show a, Testable b) => Gen a -> (a -> b) -> Property
forAll gen body = Prop $
  do a   <- gen
     res <- evaluate (body a)
     return (argument a res)
 where
  argument a res = res{ arguments = show a : arguments res }
```

Which in turn depends on:

``` haskell
evaluate :: Testable a => a -> Gen Result
evaluate a = gen where Prop gen = property a
```

One last construct for writing properties that we need is the ability to
add assumptions or pre-conditions about the input:

``` haskell
(==>) :: Testable a => Bool -> a -> Property
True  ==> a = property a
False ==> a = property ()
```

Notice how if the input doesn't pass this test, then it will be
discarded.

#### Collecting statistics

``` haskell
label :: Testable a => String -> a -> Property
label s a = Prop (add `fmap` evaluate a)
 where
  add res = res{ stamp = s : stamp res }

classify :: Testable a => Bool -> String -> a -> Property
classify True  name = label name
classify False _    = property
```

#### Running the tests

``` haskell
data Config = Config
  { maxTest :: Int
  , maxFail :: Int
  , size    :: Int -> Int
  , every   :: Int -> [String] -> String
  }

quick :: Config
quick = Config
  { maxTest = 100
  , maxFail = 1000
  , size    = (+ 3) . (`div` 2)
  , every   = \n args -> let s = show n in s ++ [ '\b' | _ <- s ]
  }

verbose :: Config
verbose = quick
  { every = \n args -> show n ++ ":\n" ++ unlines args
  }
```

``` haskell
test, quickCheck, verboseCheck :: Testable a => a -> IO ()
test         = check quick
quickCheck   = check quick
verboseCheck = check verbose

check :: Testable a => Config -> a -> IO ()
check config a =
  do rnd <- newStdGen
     tests config (evaluate a) rnd 0 0 []

tests :: Config -> Gen Result -> StdGen -> Int -> Int -> [[String]] -> IO ()
tests config gen rnd0 ntest nfail stamps
  | ntest == maxTest config = do done "OK, passed" ntest stamps
  | nfail == maxFail config = do done "Arguments exhausted after" ntest stamps
  | otherwise               =
      do putStr (every config ntest (arguments result))
         case ok result of
           Nothing    ->
             tests config gen rnd1 ntest (nfail+1) stamps
           Just True  ->
             tests config gen rnd1 (ntest+1) nfail (stamp result:stamps)
           Just False ->
             putStr ( "Falsifiable, after "
                   ++ show ntest
                   ++ " tests:\n"
                   ++ unlines (arguments result)
                    )
     where
      result      = generate (size config ntest) rnd2 gen
      (rnd1,rnd2) = split rnd0
```

### The extension to add coverage-guidance

Okey, so the above is the first version of the original property-based
testing tool, QuickCheck. Now let's add coverage-guidence to it!

The function that checks a property with coverage-guidance slight
different from `quickCheck`[^7]:

``` haskell
coverCheck :: (Arbitrary a, Show a) => Config -> ([a] -> Property)  -> IO ()
coverCheck config prop = do
  rnd <- newStdGen
  testsC config arbitrary prop [] 0 rnd 0 0 []
```

In particular notice that instead of `Testable a` we use an explicit
predicate on a list of `a`, `[a] -> Property`. The reason for using a
list in the predicate is so that we can iteratively make progress, using
the coverage information. We see this more clearly if we look at the
coverage-guided analogue of the `tests` function, in particular the `xs`
parameter:

``` haskell
testsC :: Show a => Config -> Gen a -> ([a] -> Property) -> [a] -> Int
       -> StdGen -> Int -> Int -> [[String]] -> IO ()
testsC config gen prop xs cov rnd0 ntest nfail stamps
  | ntest == maxTest config = do done "OK, passed" ntest stamps
  | nfail == maxFail config = do done "Arguments exhausted after" ntest stamps
  | otherwise               =
      do putStr (every config ntest (arguments result))
         case ok result of
           Nothing    ->
             testsC config gen prop xs cov rnd1 ntest (nfail+1) stamps
           Just True  -> do
             let stamps' = stamp result : stamps
                 cov'    = length (nub (concat stamps'))
             if cov' > cov
             then testsC config gen prop xs' cov' rnd1 (ntest+1) nfail stamps'
             else testsC config gen prop xs  cov  rnd1 (ntest+1) nfail stamps'
           Just False ->
             putStrLn ( "Falsifiable, after "
                   ++ show ntest
                   ++ " tests:\n"
                   ++ head (arguments result)
                    )
     where
       x              = generate (size config ntest) rnd3 gen
       xs'            = xs ++ [x]
       Prop genResult = prop xs'
       result_        = generate (size config ntest) rnd4 genResult
       result         = result_ {arguments = show xs' : arguments result_ }
       (rnd1,rnd2)    = split rnd0
       (rnd3,rnd4)    = split rnd2
```

The other important difference is the `cov`erage parameter, which keeps
track of how many things have been `classify`ed (the `stamps`
parameter). Notice how we only add the newly generated input, `x`, if
the `cov`erage increases.

## Example test runs using the prototype

### Traditional use of coverage

Let's start by having a look at how one would typically write a property
using vanilla `quickCheck`. Consider `insert`ing into an already sorted
list:

``` haskell
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y : xs) | x <= y    = x : y : xs
                  | otherwise = y : insert x xs
```

If we do so, then we resulting list should remain sorted:

``` haskell
prop_insert :: Int -> [Int] -> Property
prop_insert x xs = isSorted xs ==> isSorted (insert x xs)

isSorted :: Ord a => [a] -> Bool
isSorted xs = sort xs == xs
```

This test passes:

    >>> quickCheck prop_insert
    OK, passed 100 tests.

What do the test cases that are generated look like? This is where
`classify` comes in:

``` haskell
prop_insert' :: Int -> [Int] -> Property
prop_insert' x xs = isSorted xs ==>
  classify (null xs) "empty" $
  classify (length xs == 1) "singleton" $
  classify (length xs > 1 && length xs <= 3) "short" $
  classify (length xs > 3) "longer" $
    isSorted (insert x xs)
```

Running this property, we get some statistics about the generated data:

    >>> quickCheck prop_insert'
    OK, passed 100 tests.
    54% empty.
    27% singleton.
    19% short.

As we can see, all of the lists that get generated are less than 3
elemens long! This is perhaps not what we expected. However if we
consider that precondition says that the list must be sorted, then it
should become clear that it's unlikely to generate such longer such
lists completely by random[^8].

### Using coverage to guide generation

We now have all the pieces to test the example from the
[motivation](#motivation) section:

``` haskell
bad :: String -> Property
bad s = coverage 0 'b'
      $ coverage 1 'a'
      $ coverage 2 'd'
      $ coverage 3 '!' $ if s == "bad!" then False else True

  where
    coverage :: Testable a => Int -> Char -> a -> Property
    coverage i ch = classify (s !? i == Just ch) [ch]

    (!?) :: [a] -> Int -> Maybe a
    xs !? i | i < length xs = Just (xs !! i)
            | otherwise     = Nothing
```

This property basically says that there's no string that's equal to
`"bad!"`, which is obviously false. If we try to test this property
using the unmodified first version of QuickCheck:

``` haskell
testBad :: IO ()
testBad = check config bad
  where
    config = quick { maxTest = (2^8)^4 }
```

We'll see spin away, but not actually find the bad string:

    >>> testBad
    32301
    ^CInterrupted.

I stopped it after about 32k tries, in theory we'd need more than 4
billion attempts to find the bad string using this approach.

Whereas if we use coverage-guided generation:

``` haskell
testBad' :: IO ()
testBad' = coverCheck config bad
  where
    config = verbose
      { maxTest = (2^8)*4
      , every = \n args -> show n ++ ": " ++ unlines args
      }
```

We find the bad string pretty quickly. I'm using verbose output here so
you can see how it first find the `"b"`, then `"ba"`, etc:

    >>> testBad'
    0: "n"
    1: "T"
    2: "L"
    3: "|"
    4: "X"
    5: "\""
    6: "e"
    7: "G"
    8: "R"
    9: "}"
    10: "C"
    11: "3"
    12: ">"
    13: "C"
    14: "J"
    15: "9"
    16: "="
    17: "9"
    18: "L"
    19: ")"
    20: "5"
    21: "6"
    22: "x"
    23: "#"
    24: "T"
    25: "T"
    26: "_"
    27: "@"
    28: "}"
    29: "y"
    30: "-"
    31: "s"
    32: "b"
    33: "bx"
    34: "b9"
    35: "bf"
    36: "bl"
    37: "ba"
    38: "baC"
    39: "baX"
    40: "baA"
    41: "baE"
    42: "bay"
    43: "baX"
    44: "ba6"
    45: "ba@"
    46: "bai"
    47: "ba}"
    48: "bay"
    49: "bac"
    50: "bak"
    51: "ba`"
    52: "bad"
    53: "bad8"
    54: "bade"
    55: "bad0"
    56: "badA"
    57: "badP"
    58: "badQ"
    59: "bad0"
    60: "bade"
    61: "bad)"
    62: "bado"
    63: "badE"
    64: "bad\""
    65: "bad@"
    66: "bad{"
    67: "badX"
    68: "bado"
    69: "badb"
    70: "bad~"
    71: "bada"
    72: "bad%"
    73: "bad9"
    74: "badE"
    75: "bad8"
    76: "bad{"
    77: "badS"
    78: "badn"
    79: "bad?"
    80: "badn"
    81: "badq"
    82: "bady"
    83: "badA"
    84: "bad4"
    85: "bad;"
    86: "bad9"
    87: "badU"
    88: "bad!"
    Falsifiable, after 88 tests:
    "bad!"

The full source code is available
[here](https://github.com/stevana/coverage-guided-pbt).

## Conclusion and further work

- Exponential -\> polynomial

- Makes more sense for stateful systems than pure functions? Or atleast
  properties that expect a sequence of inputs?

- Don't rerun all commands for every newly generate command

  - only reset the system when shrinking

- Problem of strategy (pick something as basis for progress): coverage,
  logs, value of memory, helps bootstap the process. Generalise to
  support more?

- Local maxima?

- Problem of tactics: picking a good input distributed for the testing
  problem at hand. Make previous input influence the next input?
  Dependent events, e.g. if one packet gets lost, there's a higher
  chance that the next packet will be lost as well.

- Save `(Coverage, Mutation, Frequency, Coverage)` stats?

- More realistic example, e.g.: leader election, transaction rollback,
  failover?

- Annoying to sprinkle sometimes assertions everywhere?

  - Can it be combined with logging or tracing?

- Use size parameter to implement AFL heuristic for choosing integers?
  Or just use `frequency`?

- Type-generic mutation?

- sometimes_each?

## See also

- <https://carstein.github.io/fuzzing/2020/04/18/writing-simple-fuzzer-1.html>
- <https://carstein.github.io/fuzzing/2020/04/25/writing-simple-fuzzer-2.html>
- <https://carstein.github.io/fuzzing/2020/05/02/writing-simple-fuzzer-3.html>
- <https://carstein.github.io/fuzzing/2020/05/21/writing-simple-fuzzer-4.html>
- [How Antithesis finds bugs (with help from the Super Mario
  Bros)](https://antithesis.com/blog/sdtalk/)

[^1]: This example is due to Dmitry Vyukov, the main author of
    [go-fuzz](https://github.com/dvyukov/go-fuzz), but it's basically an
    easier to understand version of the example from Dan Luu's post. For
    comparison, here's Dan's example in full:

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

[^2]: For more details about how it works, see the [AFL
    "whitepaper"](https://lcamtuf.coredump.cx/afl/technical_details.txt)
    and its [mutation
    heuristics](https://lcamtuf.blogspot.com/2014/08/binary-fuzzing-strategies-what-works.html).

[^3]: As I was writing up, I stumbled across the paper [*Ijon: Exploring
    Deep State Spaces via
    Fuzzing*](https://ieeexplore.ieee.org/document/9152719) (2020) which
    lets the user to add custom coverage annotations. HypoFuzz also has
    this
    [functionality](https://hypofuzz.com/docs/configuration.html#custom-coverage-events).

[^4]: See the appendix of the original
    [paper](https://dl.acm.org/doi/10.1145/351240.351266) that first
    introduced property-based testing. It's interesting to note that the
    collecting statistics functionality is older than shrinking.

[^5]: We'll not talk about the `coarbitrary` method of the `Arbitrary`
    type class, which is used to generate functions, in this post.

[^6]: The reason for wrapping `Gen` in the `Arbitrary` type class is so
    that generators don't have to be passed explicitly. Not everyone
    agrees that this is a good idea, as type class instances cannot be
    managed by the module system.

[^7]: It might be interesting to note that we can implement this
    signature using the original combinators:

    ``` haskell
    testsC' :: Show a => Config -> Gen a -> ([a] -> Property)
            -> StdGen -> Int -> Int -> [[String]] -> IO ()
    testsC' config gen prop = tests config genResult
      where
        Prop genResult = forAll (genList gen) prop
        genList gen = sized $ \len -> replicateM len gen
    ```

[^8]: The standard workaround here is to introduce a wrapper type for
    which we write a custom generator which generates a random list and
    then sorts it before returning. That way no pre-condition is needed,
    as the input will be sorted by construction so to say.
