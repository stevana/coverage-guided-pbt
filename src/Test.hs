{-# LANGUAGE ScopedTypeVariables #-}

module Test where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import System.Random

import Coverage
import Generator
import Shrinker

------------------------------------------------------------------------

-- start snippet check
type Seed = Int

type Shrinking = Bool

checkM :: forall a c. (Show a, Show c)
       => Seed -> Int -> Gen a -> (Shrinking -> Coverage c -> [a] -> IO Bool) -> IO ()
checkM seed numTests gen p = do
  coverage <- emptyCoverage
  mShrinkSteps <- go numTests coverage 0 []
  case mShrinkSteps of
    Nothing -> do
      putStrLn "\nOK"
      cov <- readCoverage coverage
      putStrLn $ "Coverage: " ++ show cov
    Just shrinkSteps -> do
      putStrLn $ "Failed: " ++ show (NonEmpty.head shrinkSteps)
      putStrLn $ "Shrinking: " ++ show shrinkSteps
      putStrLn $ "#Shrinks: " ++ show (NonEmpty.length shrinkSteps - 1)
      putStrLn $ "Shrunk: " ++ show (NonEmpty.last shrinkSteps)
      cov <- readCoverage coverage
      putStrLn $ "Coverage: " ++ show cov
  where
    go :: Int -> Coverage c -> Int -> [a] -> IO (Maybe (NonEmpty [a]))
    go 0 _cov _before _cmds = return Nothing
    go n _cov  before  cmds = do
      let sz  = n * 3 `div` 2
      let cmd = generate sz (mkStdGen (seed + n)) gen
      cmds' <- randomMutation cmds cmd
      (ok, after) <- withCoverage (\cov -> p False cov cmds')
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
        Just <$> shrinker (p True _cov) (shrinkList (const [])) cmds'
-- end snippet

------------------------------------------------------------------------

-- start snippet mutate
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
-- end snippet
