# Coverage-guided property-based testing

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

- Hypothesis

  - Has notion of coverage:
    <https://hypothesis.readthedocs.io/en/latest/details.html#hypothesis.event>)
  - But coverage-guided testing was
    [removed](https://github.com/HypothesisWorks/hypothesis/pull/1564/commits/dcbea9148be3446392bc3af8892d49f3cc74fbe3)

- [Crowbar](https://github.com/stedolan/crowbar)

- [FuzzChick](https://dl.acm.org/doi/10.1145/3360607)?

- Shae "shapr" Erisson's post [*Run property tests until coverage stops
  increasing*](https://shapr.github.io/posts/2023-07-30-goldilocks-property-tests.html) (2023)
  and [trynocular](https://github.com/shapr/trynocular) library.

## Examples and the main idea of coverage-guidance

To get a better understanding of why coverage-guidence is needed, let's
consider a concrete example.

This example is due to Dmitry Vyukov, the main author of
[go-fuzz](https://github.com/dvyukov/go-fuzz), but it's basically an
easier to understand version of the example from Dan's post[^1].

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

Even if we generate random inputs of exactly the length 4, it would
still take
$O(2^8 \cdot 2^8 \cdot 2^8 \cdot 2^8) = O((2^8)^4) = O(2^{32}) = 4294967296$
tries to trigger the bug (and obviously even longer if we tried arrays
of varying length).

With coverage-guidance we keep track of inputs that resulted in
increased coverage. So, for example, if we generate the array
`[]byte{'A'}` we get further into the nested ifs, and so we take note of
that and start generating longer arrays that start with 'A' and see if
we get even further, etc.

By building on previous succeses in getting more coverage, we can
effectively reduce the problem to only need
$O(2^8 + 2^8 + 2^8 + 2^8) = O(2^8 \cdot 4) =
O(2^{10}) = 1024$ tries. With other words coverage-guidence turns an
exponential problem into a polynomial problem!

Great, but where do we get this coverage information from?

AFL and `go-fuzz` both get it from the compiler.

When I've been thinking about how to implement coverage-guided
property-based testing in the past, I always got stuck thinking that
parsing the coverage output from the compiler in between test case
generation rounds would be annoying and slow.

I didn't know that you could get this information from a library
provided by the GHC compiler in Haskell, until I read Shae "shapr"
Erisson does in his
[post](https://shapr.github.io/posts/2023-07-30-goldilocks-property-tests.html).

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
idea that this post adds, is that property-based testing already has
functionality for implementing "sometimes assertions": the `label`,
`classify` and `collect` machinary for gathering run-time statistics of
the generated data!

This machinary is [crucial](https://www.youtube.com/watch?v=NcJOiQlzlXQ)
for writing good tests and has been part of the QuickCheck
implementation since the very first version[^2]!

So the question is: can we implement coverage-guided property-based
testing using the internal notion of coverage that property-based
testing already has?

## Prototype implementation

- Edsko de Vries'
  [Mini-QuickCheck](https://www.well-typed.com/blog/2019/05/integrated-shrinking/)

``` haskell
type Seed = Int

checkM :: forall a c. (Show a, Show c)
       => Seed -> Int -> Integrated a -> IO () -> ([a] -> Coverage c -> IO Bool) -> IO ()
checkM seed numTests gen reset p = do
  coverage <- emptyCoverage
  mShrinkSteps <- go numTests coverage 0 []
  case mShrinkSteps of
    Nothing -> do
      putStrLn "\nOK"
      cov <- readCoverage coverage
      putStrLn $ "Coverage: " ++ show cov
    Just shrinkSteps -> do
      putStrLn $ "Failed: " ++ case shrinkSteps of
                                 [] -> error "impossible: shrinkSteps empty"
                                 (s : _ss) -> show s
      putStrLn $ "Shrinking: " ++ show shrinkSteps
      putStrLn $ "Shrunk: " ++ show (last shrinkSteps)
      cov <- readCoverage coverage
      putStrLn $ "Coverage: " ++ show cov
  where
    go :: Int -> Coverage c -> Int -> [a] -> IO (Maybe [[a]])
    go 0 _cov _before _cmds = return Nothing
    go n _cov  before  cmds = do

      let cmd = root $ runIntegrated (mkStdGen (seed + n)) gen
      cmds' <- randomMutation cmds cmd
      (ok, after) <- withCoverage (p cmds')
      if ok
      then do
        let diff = compareCoverage before after
        case diff of
          Increased -> do
            -- putStr "p"
            go (n - 1) _cov after cmds'
          Same      -> do
            -- putStr "p"
            go (n - 1) _cov after cmds'
          Decreased -> do
            -- putStr "."
            go (n - 1) _cov before cmds
      else do
        putStrLn "\n(Where `p` and `.` indicate picked and dropped values respectively.)"
        Just <$> minimise (flip p _cov) reset (unfoldTree (shrinkList (const [])) cmds')
```

``` haskell
minimise :: (a -> IO Bool) -> IO () -> Tree a -> IO [a]
minimise p reset (Node x xs) = do
  xs' <- filterM (\x' -> reset >> fmap not (p (root x'))) xs
  case xs' of
    []   -> return [x]
    x':_ -> (:) <$> pure x <*> minimise p reset x'
```

``` haskell
randomMutation :: [a] -> a -> IO [a]
randomMutation [] x = return [x]
randomMutation xs x = do
  appendOrUpdate <- randomIO -- XXX: nondet
  if appendOrUpdate
  then return (xs ++ [x])
  else do
    ix <- randomRIO (0, length xs - 1)
    return (update ix xs x)
  where
    update :: Int -> [a] -> a -> [a]
    update ix xs0 x' = case splitAt ix xs0 of
      (before, _x : after) -> before ++ x' : after
      (_, []) -> error "update: impossible"
```

``` haskell
newtype Coverage a = Coverage (IORef (Set a))

emptyCoverage :: IO (Coverage a)
emptyCoverage = Coverage <$> newIORef Set.empty

addCoverage :: Ord a => Coverage a -> a -> IO ()
addCoverage (Coverage ref) x = modifyIORef' ref (Set.insert x)

readCoverage :: Coverage a -> IO (Set a)
readCoverage (Coverage ref) = readIORef ref

checkCoverage :: Coverage a -> IO Int
checkCoverage (Coverage ref) = Set.size <$> readIORef ref

data CoverageDiff = Decreased | Same | Increased

compareCoverage :: Int -> Int -> CoverageDiff
compareCoverage before after = case compare after before of
  LT -> Decreased
  EQ -> Same
  GT -> Increased

withCoverage :: (Coverage c -> IO a) -> IO (a, Int)
withCoverage k = do
  c <- emptyCoverage
  x <- k c
  after <- checkCoverage c
  return (x, after)
```

``` haskell
newtype Gen a = Gen (StdGen -> a)

instance Functor Gen where
  fmap f (Gen k) = Gen (f . k)

runGen :: StdGen -> Gen a -> a
runGen prng (Gen g) = g prng

instance Applicative Gen where
  pure x = Gen $ \_prng -> x
  (<*>)  = ap

instance Monad Gen where
  return  = pure
  x >>= f = Gen $ \prng ->
    let (prngX, prngF) = split prng
    in runGen prngF (f (runGen prngX x))
```

The full source code is available
[here](https://github.com/stevana/coverage-guided-pbt).

## Testing some examples with the prototype

## Conclusion and further work

- More realistic example, e.g.: leader election, transaction rollback,
  failover?

- Annoying to sprinkle sometimes assertions everywhere?

  - Can it be combined with logging or tracing?

- Use size parameter to implement AFL heuristic for choosing integers?
  Or just use `frequency`?

[^1]: Here's Dan's example in full:

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

    As I hope we can agree, it's very similar to Dmitry's example,
    except it's a bit less clear what exactly happens in the if
    statement.

[^2]: See the appendix of the original
    [paper](https://dl.acm.org/doi/10.1145/351240.351266) that first
    introduced property-based testing. It's interesting to note that
    this functionality is older than shrinking.
