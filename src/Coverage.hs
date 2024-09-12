module Coverage where

import Data.IORef
import Data.Set (Set)
import qualified Data.Set as Set

------------------------------------------------------------------------

newtype Coverage a = Coverage (IORef (Set a))

emptyCoverage :: IO (Coverage a)
emptyCoverage = Coverage <$> newIORef Set.empty

clearCoverage :: Coverage a -> IO ()
clearCoverage (Coverage ref) = writeIORef ref Set.empty

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
{-
withCoverageDiff :: Coverage c -> IO a -> IO (a, CoverageDiff)
withCoverageDiff cov io = do
  before <- checkCoverage cov
  clearCoverage cov
  x <- io
  after <- checkCoverage cov
  let diff = compareCoverage before after
  return (x, diff)

 -}
