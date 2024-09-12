{-# LANGUAGE ScopedTypeVariables #-}

module Test where

import Control.Monad
import Data.IORef
import Data.Set (Set)
import qualified Data.Set as Set
import System.Random

import Generate
import Generate.Tree

------------------------------------------------------------------------

type Seed = Int

newtype Coverage a = Coverage (IORef (Set a))

emptyCoverage :: IO (Coverage a)
emptyCoverage = Coverage <$> newIORef Set.empty

addCoverage :: Ord a => Coverage a -> a -> IO ()
addCoverage (Coverage ref) x = modifyIORef' ref (Set.insert x)

readCoverage :: Coverage a -> IO (Set a)
readCoverage (Coverage ref) = readIORef ref

checkCoverage :: Coverage a -> IO Int
checkCoverage (Coverage ref) = Set.size <$> readIORef ref

------------------------------------------------------------------------

checkM :: forall a c. (Show a, Show c)
       => Seed -> Int -> Integrated a -> ([a] -> Coverage c -> IO Bool) -> IO ()
checkM seed numTests gen p = do
  coverage <- emptyCoverage
  mShrinkSteps <- go numTests coverage []
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
    go :: Int -> Coverage c -> [a] -> IO (Maybe [[a]])
    go 0 _cov _cmds = return Nothing
    go n  cov  cmds = do
      let cmd = root $ runIntegrated (mkStdGen (seed + n)) gen
      let cmds' = cmds ++ [cmd]
      before <- checkCoverage cov
      ok <- p cmds' cov
      after <- checkCoverage cov
      if ok
      then do
        let improvedCoverage = before < after
        if improvedCoverage
        then do
          putStr "p"
          go (n - 1) cov cmds'
        else do
          putStr "."
          go (n - 1) cov cmds
      else do
        putStrLn "\n(Where `p` and `.` indicate picked and dropped values respectively.)"
        Just <$> minimise (flip p cov) (unfoldTree (shrinkList (const [])) cmds')

minimise :: (a -> IO Bool) -> Tree a -> IO [a]
minimise p (Node x xs) = do
  xs' <- filterM (fmap not . p . root) xs
  case xs' of
    []   -> return [x]
    x':_ -> (:) <$> pure x <*> minimise p x'
