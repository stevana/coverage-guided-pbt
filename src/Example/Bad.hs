module Example.Bad where

import System.Random

import Checker
import Coverage
import Generator
import Mutator

------------------------------------------------------------------------

bad :: Coverage Char -> String -> IO Bool
bad cov s = do
  if_ 0 'b' $
    if_ 1 'a' $
      if_ 2 'd' $
        if_ 3 '!' $
          return False
  where
    if_ :: Int -> Char -> IO Bool -> IO Bool
    if_ ix ch tt | safeIndex s ix == Just ch = do
        addCoverage cov ch
        tt
      | otherwise = return True

safeIndex :: [a] -> Int -> Maybe a
safeIndex xs ix | ix >= length xs = Nothing
                | otherwise       = Just (xs !! ix)

testBad :: IO ()
testBad = do
  seed <- randomIO
  putStrLn ("Seed: " ++ show seed)
  let cfg = Config seed 100
      arb = Arbitrary genChar (const []) mutateChar
  checker cfg arb bad
