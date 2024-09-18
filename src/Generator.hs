module Generator where

import Control.Monad
import Data.Char
import System.Random

--------------------------------------------------------------------

-- start snippet Gen
newtype Gen a = Gen (Size -> StdGen -> a)

type Size = Int
-- end snippet

sized :: (Int -> Gen a) -> Gen a
sized fgen = Gen (\n r -> let Gen m = fgen n in m n r)

rand :: Gen StdGen
rand = Gen (\_sz prng -> prng)

generate :: Int -> StdGen -> Gen a -> a
generate n prng (Gen m) = m sz prng'
  where
    (sz, prng') = randomR (0, n) prng

instance Functor Gen where
  fmap f m = m >>= return . f

instance Applicative Gen where
    pure x = Gen $ \_sz _prng -> x
    Gen f' <*> Gen x' = Gen $ \sz r0 ->
        let (r1,r2) = split r0
            f = f' sz r1
            x = x' sz r2
        in f x

instance Monad Gen where
  return      = pure
  Gen m >>= k =
    Gen (\sz r0 -> let (r1,r2) = split r0
                       Gen m'  = k (m sz r1)
                   in m' sz r2)

-- derived

choose :: Random a => (a, a) -> Gen a
choose bounds = (fst . randomR bounds) `fmap` rand

elements :: [a] -> Gen a
elements xs = (xs !!) `fmap` choose (0, length xs - 1)

vector :: Int -> Gen a -> Gen [a]
vector n gen = sequence [ gen | i <- [1..n] ]

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

------------------------------------------------------------------------

genChar :: Gen Char
genChar = fmap chr $ (fst . randomR (0, 128)) <$> rand

genSmallLetter :: Gen Char
genSmallLetter = fmap chr $ (fst . randomR (97, 122)) <$> rand

genList :: Gen a -> Gen [a]
genList g = sized $ \sz -> vector sz g

genIntegral :: (Integral a, Random a) => a -> a -> Gen a
genIntegral lo hi = choose (lo, hi)
