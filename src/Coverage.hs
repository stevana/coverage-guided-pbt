module Coverage where

import Data.IORef
import Data.Set (Set)
import qualified Data.Set as Set

------------------------------------------------------------------------

-- start snippet Coverage
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
  deriving Eq

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
-- end snippet
--
withCoverage' :: (Coverage c -> IO a) -> IO (a, Coverage c, Int)
withCoverage' k = do
  c <- emptyCoverage
  x <- k c
  after <- checkCoverage c
  return (x, c, after)
