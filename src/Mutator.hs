{-# LANGUAGE ScopedTypeVariables #-}

module Mutator where

import Data.Char
import Data.Int
import System.Random

import Generator

------------------------------------------------------------------------

type Mutate a = StdGen -> a -> a

mutateChar :: Mutate Char
mutateChar prng ch =  generate 0 prng genChar
-- if ch == 'A' then 'Z' else pred ch

mutateInt16 :: Mutate Int16
mutateInt16 prng _i = fst (random prng) -- XXX: this doesn't actually mutate...

data ListMutation = Update | Take | Append
  deriving (Bounded, Enum)

randomFinite :: forall g a. (RandomGen g, Bounded a, Enum a) => g -> (a, g)
randomFinite prng =
  let
    lo = fromEnum (minBound :: a)
    hi = fromEnum (maxBound :: a)
    (i, prng') = randomR (lo, hi) prng
  in
    (toEnum i, prng')

randomREnum :: (RandomGen g, Enum a) => (a, a) -> g -> (a, g)
randomREnum (lo, hi) prng =
  let
    (i, prng') = randomR (fromEnum lo, fromEnum hi) prng
  in
    (toEnum i, prng')

instance Random ListMutation where
  random  = randomFinite
  randomR = randomREnum

mutateList :: Int -> Gen a -> Mutate a -> Mutate [a]
mutateList sz gen _mut prng [] = [generate sz prng gen]
mutateList sz gen  mut prng xs =
  let
    (op, prng') = random prng
  in
    case op of
      Update ->
        let
          (ix, prng'') = randomR (0, length xs - 1) prng'
        in
          update ix xs (mut prng'')
      Take ->
        -- XXX: We probably want to throw away approximately half of the list
        -- most of the time...
        let
          n = fst (randomR (length xs - 1 `div` 2, length xs - 1) prng')
        in
          take n xs
      Append -> xs ++ [generate sz prng gen]
  where
    update :: Int -> [a] -> (a -> a) -> [a]
    update ix xs0 f = case splitAt ix xs0 of
      (before, x : after) -> before ++ f x : after
      (_, []) -> error "update: impossible"

mutateFinite :: forall a. (Bounded a, Enum a) => a -> a
mutateFinite x = toEnum ((fromEnum x + 1) `mod` (fromEnum (maxBound :: a) + 1))


