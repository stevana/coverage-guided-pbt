module QuickCheckV1
  -- testing functions
  ( quickCheck    -- :: prop -> IO ()
  , verboseCheck  -- :: prop -> IO ()
  , test          -- :: prop -> IO ()  -- = quickCheck

  , Config(..)    -- :: *
  , check         -- :: Config -> prop -> IO ()

  -- property combinators
  , forAll        -- :: Gen a -> (a -> prop) -> prop
  , (==>)         -- :: Bool -> prop -> prop

  -- gathering test-case information
  , label         -- :: String         -> prop -> prop
  , collect       -- :: Show a => a    -> prop -> prop
  , classify      -- :: Bool -> String -> prop -> prop
  , trivial       -- :: Bool           -> prop -> prop

  -- generator combinators
  , Gen           -- :: * -> * ; Functor, Monad

  , elements      -- :: [a] -> Gen a
  , two           -- :: Gen a -> Gen (a,a)
  , three         -- :: Gen a -> Gen (a,a,a)
  , four          -- :: Gen a -> Gen (a,a,a,a)

  , sized         -- :: (Int -> Gen a) -> Gen a
  , resize        -- :: Int -> Gen a -> Gen a
  , choose        -- :: Random a => (a, a) -> Gen a
  , oneof         -- :: [Gen a] -> Gen a
  , frequency     -- :: [(Int, Gen a)] -> Gen a

  , vector        -- :: Arbitrary a => Int -> Gen [a]

  -- default generators
  , Arbitrary(..) -- :: class
  , rand          -- :: Gen StdGen
  , promote       -- :: (a -> Gen b) -> Gen (a -> b)
  , variant       -- :: Int -> Gen a -> Gen a

  -- testable
  , Testable(..)  -- :: class
  , Property      -- :: *

  , testBad
  , testBad'
  , prop_insert
  , prop_insert'
  )
 where

-- QuickCheck v.0.2
-- DRAFT implementation; last update 000104.
-- Koen Claessen, John Hughes.

import Control.Monad (liftM2, liftM3, liftM4, replicateM)
import Data.Char
import Data.List (group, intersperse, nub, sort)
import System.Random

infixr 0 ==>
infix  1 `classify`

--------------------------------------------------------------------
-- Generator

-- start snippet Gen
newtype Gen a = Gen (Int -> StdGen -> a)
-- end snippet

-- start snippet rand
rand :: Gen StdGen
rand = Gen (\_n r -> r)

sized :: (Int -> Gen a) -> Gen a
sized fgen = Gen (\n r -> let Gen m = fgen n in m n r)
-- end snippet

resize :: Int -> Gen a -> Gen a
resize n (Gen m) = Gen (\_ r -> m n r)


promote :: (a -> Gen b) -> Gen (a -> b)
promote f = Gen (\n r -> \a -> let Gen m = f a in m n r)

variant :: Int -> Gen a -> Gen a
variant v (Gen m) = Gen (\n r -> m n (rands r !! (v+1)))
 where
  rands r0 = r1 : rands r2 where (r1, r2) = split r0

-- start snippet Gen

generate :: Int -> StdGen -> Gen a -> a
generate n rnd (Gen m) = m size rnd'
 where
  (size, rnd') = randomR (0, n) rnd
-- end snippet

instance Functor Gen where
  fmap f m = m >>= return . f

instance Applicative Gen where
    pure x = Gen $ \n r -> x
    Gen f' <*> Gen x' = Gen $ \n r0 ->
        let (r1,r2) = split r0
            f = f' n r1
            x = x' n r2
        in f x

instance Monad Gen where
  return a    = Gen (\n r -> a)
  Gen m >>= k =
    Gen (\n r0 -> let (r1,r2) = split r0
                      Gen m'  = k (m n r1)
                   in m' n r2)

-- derived

-- start snippet derivedCombinators
choose :: Random a => (a, a) -> Gen a
choose bounds = (fst . randomR bounds) `fmap` rand

elements :: [a] -> Gen a
elements xs = (xs !!) `fmap` choose (0, length xs - 1)

vector :: Arbitrary a => Int -> Gen [a]
vector n = sequence [ arbitrary | _i <- [1..n] ]
-- end snippet
--
oneof :: [Gen a] -> Gen a
oneof gens = elements gens >>= id

frequency :: [(Int, Gen a)] -> Gen a
frequency xs = choose (1, tot) >>= (`pick` xs)
 where
  tot = sum (map fst xs)

  pick n ((k,x):xs)
    | n <= k    = x
    | otherwise = pick (n-k) xs

-- general monadic

two :: Monad m => m a -> m (a, a)
two m = liftM2 (,) m m

three :: Monad m => m a -> m (a, a, a)
three m = liftM3 (,,) m m m

four :: Monad m => m a -> m (a, a, a, a)
four m = liftM4 (,,,) m m m m

--------------------------------------------------------------------
-- Arbitrary

-- start snippet Arbitrary
class Arbitrary a where
  arbitrary :: Gen a

-- end snippet
  coarbitrary :: a -> Gen b -> Gen b

instance Arbitrary () where
  arbitrary = return ()
  coarbitrary _ = variant 0

-- start snippet Arbitrary
instance Arbitrary Bool where
  arbitrary = elements [True, False]

-- end snippet
  coarbitrary b = if b then variant 0 else variant 1

-- start snippet Arbitrary
instance Arbitrary Char where
  -- Avoids generating control characters.
  arbitrary = choose (32,126) >>= \n -> return (chr n)

-- end snippet
  coarbitrary n = variant (ord n)

-- start snippet Arbitrary
instance Arbitrary Int where
  arbitrary = sized $ \n -> choose (-n,n)

-- end snippet
  coarbitrary n = variant (if n >= 0 then 2*n else 2*(-n) + 1)

instance Arbitrary Integer where
  arbitrary = sized $ \n -> choose (-toInteger n, toInteger n)
  coarbitrary n = variant (fromInteger (if n >= 0 then 2*n else 2*(-n) + 1))

instance Arbitrary Float where
  arbitrary     = liftM3 fraction arbitrary arbitrary arbitrary
  coarbitrary x = coarbitrary (decodeFloat x)

instance Arbitrary Double where
  arbitrary     = liftM3 fraction arbitrary arbitrary arbitrary
  coarbitrary x = coarbitrary (decodeFloat x)

fraction a b c = fromInteger a + (fromInteger b / (abs (fromInteger c) + 1))

instance (Arbitrary a, Arbitrary b) => Arbitrary (a, b) where
  arbitrary          = liftM2 (,) arbitrary arbitrary
  coarbitrary (a, b) = coarbitrary a . coarbitrary b

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (a, b, c) where
  arbitrary             = liftM3 (,,) arbitrary arbitrary arbitrary
  coarbitrary (a, b, c) = coarbitrary a . coarbitrary b . coarbitrary c

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
      => Arbitrary (a, b, c, d)
 where
  arbitrary = liftM4 (,,,) arbitrary arbitrary arbitrary arbitrary
  coarbitrary (a, b, c, d) =
    coarbitrary a . coarbitrary b . coarbitrary c . coarbitrary d

-- start snippet Arbitrary
instance Arbitrary a => Arbitrary [a] where
  arbitrary = sized (\n -> choose (0,n) >>= vector)

-- end snippet
  coarbitrary []     = variant 0
  coarbitrary (a:as) = coarbitrary a . variant 1 . coarbitrary as

instance (Arbitrary a, Arbitrary b) => Arbitrary (a -> b) where
  arbitrary = promote (`coarbitrary` arbitrary)
  coarbitrary f gen = arbitrary >>= ((`coarbitrary` gen) . f)

--------------------------------------------------------------------
-- Testable

-- start snippet Result
data Result
  = Result { ok :: Maybe Bool, stamp :: [String], arguments :: [String] }

nothing :: Result
nothing = Result{ ok = Nothing, stamp = [], arguments = [] }
-- end snippet

-- start snippet Property
newtype Property
  = Prop (Gen Result)

result :: Result -> Property
result res = Prop (return res)
-- end snippet

-- start snippet evaluate
evaluate :: Testable a => a -> Gen Result
evaluate a = gen where Prop gen = property a
-- end snippet

-- start snippet Testable
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
-- end snippet

-- start snippet forAll
forAll :: (Show a, Testable b) => Gen a -> (a -> b) -> Property
forAll gen body = Prop $
  do a   <- gen
     res <- evaluate (body a)
     return (argument a res)
 where
  argument a res = res{ arguments = show a : arguments res }
-- end snippet

-- start snippet assuming
(==>) :: Testable a => Bool -> a -> Property
True  ==> a = property a
False ==> a = property ()
-- end snippet

-- start snippet classify
label :: Testable a => String -> a -> Property
label s a = Prop (add `fmap` evaluate a)
 where
  add res = res{ stamp = s : stamp res }

classify :: Testable a => Bool -> String -> a -> Property
classify True  name = label name
classify False _    = property
-- end snippet

trivial :: Testable a => Bool -> a -> Property
trivial = (`classify` "trivial")

collect :: (Show a, Testable b) => a -> b -> Property
collect v = label (show v)

--------------------------------------------------------------------
-- Testing

-- start snippet Config
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
-- end snippet

-- start snippet quickCheck
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
-- end snippet

done :: String -> Int -> [[String]] -> IO ()
done mesg ntest stamps =
  do putStr ( mesg ++ " " ++ show ntest ++ " tests" ++ table )
 where
  table = display
        . map entry
        . reverse
        . sort
        . map pairLength
        . group
        . sort
        . filter (not . null)
        $ stamps

  display []  = ".\n"
  display [x] = " (" ++ x ++ ").\n"
  display xs  = ".\n" ++ unlines (map (++ ".") xs)

  pairLength xss@(xs:_) = (length xss, xs)
  entry (n, xs)         = percentage n ntest
                       ++ " "
                       ++ concat (intersperse ", " xs)

  percentage n m        = show ((100 * n) `div` m) ++ "%"

--------------------------------------------------------------------
-- the end.

-- start snippet testsC2
testsC' :: Show a => Config -> Gen a -> ([a] -> Property)
        -> StdGen -> Int -> Int -> [[String]] -> IO ()
testsC' config gen prop = tests config genResult
  where
    Prop genResult = forAll (genList gen) prop
    genList gen = sized $ \len -> replicateM len gen
-- end snippet

-- start snippet coverCheck
coverCheck :: (Arbitrary a, Show a) => Config -> ([a] -> Property)  -> IO ()
coverCheck config prop = do
  rnd <- newStdGen
  testsC config arbitrary prop [] 0 rnd 0 0 []
-- end snippet

-- start snippet testsC1
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
-- end snippet

------------------------------------------------------------------------

-- start snippet bad
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
-- end snippet

-- start snippet testBad1
testBad :: IO ()
testBad = check config bad
  where
    config = quick { maxTest = (2^8)^4 }
-- end snippet

-- start snippet testBad2
testBad' :: IO ()
testBad' = coverCheck config bad
  where
    config = verbose
      { maxTest = (2^8)*4
      , every = \n args -> show n ++ ": " ++ unlines args
      }
-- end snippet

------------------------------------------------------------------------

-- start snippet insert
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y : xs) | x <= y    = x : y : xs
                  | otherwise = y : insert x xs
-- end snippet

-- start snippet prop_insert1
prop_insert :: Int -> [Int] -> Property
prop_insert x xs = isSorted xs ==> isSorted (insert x xs)

isSorted :: Ord a => [a] -> Bool
isSorted xs = sort xs == xs
-- end snippet

-- start snippet prop_insert2
prop_insert' :: Int -> [Int] -> Property
prop_insert' x xs = isSorted xs ==>
  classify (null xs) "empty" $
  classify (length xs == 1) "singleton" $
  classify (length xs > 1 && length xs <= 3) "short" $
  classify (length xs > 3) "longer" $
    isSorted (insert x xs)
-- end snippet
