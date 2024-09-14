{-# LANGUAGE ScopedTypeVariables #-}

module Test where

import Control.Monad
import System.Random

import Coverage
import Generate
import Generate.Tree

------------------------------------------------------------------------

-- start snippet check
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
-- end snippet

-- start snippet shrink
minimise :: (a -> IO Bool) -> IO () -> Tree a -> IO [a]
minimise p reset (Node x xs) = do
  xs' <- filterM (\x' -> reset >> fmap not (p (root x'))) xs
  case xs' of
    []   -> return [x]
    x':_ -> (:) <$> pure x <*> minimise p reset x'
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
