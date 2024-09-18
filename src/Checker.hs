{-# LANGUAGE ScopedTypeVariables #-}

module Checker where

import Data.Function
import Data.List (genericLength, unfoldr)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Time.Clock
import System.Random

import Coverage
import Generator
import Mutator
import Queue
import Shrinker

------------------------------------------------------------------------

type Seed = Int

data Config = Config
  { _seed     :: Seed
  , _numTests :: Int
  }

data Arbitrary a = Arbitrary
  { _gen    :: Gen a
  , _shrink :: a -> [a]
  , _mutate :: Mutate a
  }

checker :: forall a c. Show a => Config -> Arbitrary a -> (Coverage c -> [a] -> IO Bool) -> IO ()
checker (Config seed numTests) (Arbitrary gen shrink mutate) prop = do
  let prng = mkStdGen seed
  result <- loop numTests prng emptyPQueue
  putStrLn (reporter result)
  where
    loop :: Int -> StdGen -> PQueue (Choice a) -> IO (Maybe (NonEmpty [a]))
    loop 0 _prng _q = return Nothing
    loop n  prng  q = do
      let (prng', prng'') = split prng
      xs <- chooseNext n prng' gen q
      print xs
      p        <- assignEnergy xs
      eq''     <- loop' xs p prng'' q
      case eq'' of
        Left ce              -> return (Just ce)
        Right (q'', prng''') -> loop (n - 1) prng''' q''
      where
        loop' :: Choice a -> Int -> StdGen -> PQueue (Choice a)
              -> IO (Either (NonEmpty [a]) (PQueue (Choice a), StdGen))
        loop' _xs 0 prng q = return (Right (q, prng))
        loop' c@(Choice xs _ _ before) p prng q = do
          let (prng', prng'') = split prng
          let xs' = mutateList n gen mutate prng' xs
          t0 <- getCurrentTime
          (ok, newCoverage, after) <- withCoverage' (flip prop xs')
          t1 <- getCurrentTime
          if ok
          then do
            let diff = compareCoverage before after
            if isInteresting xs' c diff
            then do
              let q' = insertBy (flip compare `on` (\c -> realToFrac (_coverage c) / genericLength (_input c))) (Choice xs' (diffUTCTime t1 t0) t0 after) q
              loop' c (p - 1) prng'' q'
            else loop' c (p - 1) prng'' q
          else Left <$> shrinker' (prop newCoverage) shrink xs'

chooseNext :: Int -> StdGen -> Gen a -> PQueue (Choice a) -> IO (Choice a)
chooseNext n prng gen q
  | isEmptyPQueue q = do
      let sz = (n * 3 `div` 2) `min` 10
      let xs = generate sz prng (genList gen)
      t0 <- getCurrentTime
      return (newChoice xs t0)
  | otherwise = do
      let lo = 0
          hi = lengthPQueue q - 1
          ix = lowerBiasedRandomR (lo, hi) (max hi 5) prng
      return (peek ix q)

lowerBiasedRandomR :: (Random a, Ord a) => (a, a) -> Int -> StdGen -> a
lowerBiasedRandomR (lo, hi) k prng =
  minimum (take k (unfoldr (Just . randomR (lo, hi)) prng))


assignEnergy :: Choice a -> IO Int
assignEnergy _ = return $ 2^16

isInteresting :: [a] -> Choice a -> CoverageDiff -> Bool
isInteresting _   _c Increased = True
isInteresting xs'  c Same      = False -- length xs' < length (_input c) -- XXX: does this improve things?
isInteresting _    _ _         = False

reporter :: Show a => Maybe (NonEmpty a) -> String
reporter Nothing = "OK"
reporter (Just shrinkSteps) = unlines
  [ "Failed:    " ++ show (NonEmpty.head shrinkSteps)
  , "Shrinking: " ++ show shrinkSteps
  , "Shrunk:    " ++ show (NonEmpty.last shrinkSteps)
  ]

data Choice a = Choice
  { _input         :: [a]
  , _executionTime :: NominalDiffTime
  , _creationTime  :: UTCTime
  , _coverage      :: Int
  }
  deriving Show

newChoice :: [a] -> UTCTime -> Choice a
newChoice xs t0 = Choice xs (fromInteger 0) t0 0
