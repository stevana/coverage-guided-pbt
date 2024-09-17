module Example.Device where

import Data.Bits
import Data.Int
import System.Random

import Coverage
import Generator
import Test

------------------------------------------------------------------------

-- Taken from https://danluu.com/testing/
{-
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
-}

someFilter :: Int16 -> Bool
someFilter x0 = go 0 x0
  where
    go 16 _x = True
    go i   x | not (x .&. 1 == 1) = False
             | otherwise          = go (i + 1) (x `shiftR` 1)

dut :: Coverage String -> [Int16] -> IO Int16
dut cov a
  | length a /= 4 = return 1
  | otherwise     = do
      addCoverage cov "a is the right length!"
      if someFilter (a !! 0)
      then do
        addCoverage cov "a[0] passed filter"
        if someFilter (a !! 1)
        then do
          addCoverage cov "a[1] passed filter"
          if someFilter (a !! 2)
          then do
            addCoverage cov "a[2] passed filter"
            if someFilter (a !! 3)
            then return 0
            else return 2
          else return 3
        else return 4
      else return 5

testD :: Seed -> IO ()
testD seed = do
  let lowerBound = minBound
      upperBound = maxBound
  -- qnbinom(0.99, 1, (1/2^16)*4) = 75448
  checkM seed 5000000 (genIntegral lowerBound upperBound) $ \_shrinking coverage a -> do
    result <- dut coverage a
    return (result /= 0)

testD_ :: IO ()
testD_ = do
  seed <- randomIO
  testD seed

