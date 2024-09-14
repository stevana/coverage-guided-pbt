module Generate where

import Control.Monad
import Data.Containers.ListUtils (nubOrd)
import Data.Int
import System.Random

import Generate.Tree

------------------------------------------------------------------------

-- start snippet Gen
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
-- end snippet

------------------------------------------------------------------------

data Manual a = Manual
  { _gen    :: Gen a
  , _shrink :: a -> [a]
  }

mBool :: Manual Bool
mBool = Manual
  { _gen    = Gen (fst . random)
  , _shrink = \b -> if b then [False] else []
  }

mInt :: Int -> Int -> Manual Int
mInt lo hi = Manual
  { _gen    = Gen (fst . randomR (lo, hi))
  , _shrink = shrinkIntegral
  }

mInt16 :: Int16 -> Int16 -> Manual Int16
mInt16 lo hi = Manual
  { _gen    = Gen (fst . randomR (lo, hi))
  , _shrink = shrinkIntegral
  }

-- | Shrink an integral number.
shrinkIntegral :: (Integral a, Ord a) => a -> [a]
shrinkIntegral x = nubOrd $
  [ -x
  | x < 0, -x > x
  ] ++
  [ x'
  | x' <- takeWhile (<< x) (0:[ x - i | i <- drop 1 (iterate (`quot` 2) x) ])
  ]
  where
    -- a << b is "morally" abs a < abs b, but taking care of overflow.
    a << b = case (a >= 0, b >= 0) of
             (True,  True)  -> a < b
             (False, False) -> a > b
             (True,  False) -> a + b < 0
             (False, True)  -> a + b > 0

mList :: Int -> Manual a -> Manual [a]
mList n (Manual gen shrink) = Manual
  { _gen    = replicateM n gen
  , _shrink = shrinkList shrink
  }

-- | Shrink a list of values given a shrinking function for individual values.
shrinkList :: (a -> [a]) -> [a] -> [[a]]
shrinkList shr xs0 = concat [ removes k n0 xs0 | k <- takeWhile (>0) (iterate (`div`2) n0) ]
                  ++ shrinkOne xs0
 where
  n0 = length xs0

  shrinkOne []     = []
  shrinkOne (x:xs) = [ x':xs | x'  <- shr x ]
                  ++ [ x:xs' | xs' <- shrinkOne xs ]

  removes k n xs
    | k > n     = []
    | null xs2  = [[]]
    | otherwise = xs2 : map (xs1 ++) (removes k (n-k) xs2)
   where
    xs1 = take k xs
    xs2 = drop k xs

------------------------------------------------------------------------

newtype Integrated a = Integrated (StdGen -> Tree a)

instance Functor Integrated where
  fmap f (Integrated k) = Integrated (fmap f . k)

instance Applicative Integrated where
  pure x = Integrated $ \_prng -> singleton x
  Integrated f <*> Integrated x = Integrated $ \prng ->
    let (prngF, prngX) = split prng
    in interleave (f prngF) (x prngX)

runIntegrated :: StdGen -> Integrated a -> Tree a
runIntegrated prng (Integrated f) = f prng

runIntegrated_ :: StdGen -> Integrated a -> a
runIntegrated_ prng (Integrated f) = root (f prng)

integrated :: Manual a -> Integrated a
integrated (Manual gen shrink) = Integrated $ \prng ->
  unfoldTree shrink $ runGen prng gen

iBool :: Integrated Bool
iBool = integrated mBool

iInt :: Int -> Int -> Integrated Int
iInt lo hi = integrated $ mInt lo hi

iInt16 :: Int16 -> Int16 -> Integrated Int16
iInt16 lo hi = integrated $ mInt16 lo hi

iPair :: Integrated a -> Integrated b -> Integrated (a, b)
iPair genA genB = (,) <$> genA <*> genB

iTriple :: Integrated a -> Integrated b -> Integrated c -> Integrated (a, b, c)
iTriple genA genB genC = (,,) <$> genA <*> genB <*> genC

iList :: Int -> Manual a -> Integrated [a]
iList n genA = integrated $ mList n genA

oneof :: [Integrated a] -> Integrated a
oneof gs = Integrated $ \prng ->
  let
    (ix, prng') = randomR (0, length gs - 1) prng
  in
    runIntegrated prng' (gs !! ix)

-- | Chooses one of the given generators, with a weighted random distribution.
-- The input list must be non-empty.
frequency :: [(Int, Integrated a)] -> Integrated a
frequency []  = error "frequency: can't be used with empty list"
frequency xs0 = Integrated $ \prng ->
  let
    (n, prng') = randomR (1, total) prng
  in
    pick n xs0 prng'
  where
    total = sum (map fst xs0)

    pick n ((k,x):xs) prng
      | n <= k    = runIntegrated prng x
      | otherwise = pick (n-k) xs prng
    pick _ _  _ = error "pick: can't be used with empty list"
