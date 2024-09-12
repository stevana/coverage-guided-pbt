module Statistics where

import Data.List

------------------------------------------------------------------------

choose :: Int -> Int -> Rational
choose n k = fromIntegral (factorial n) / fromIntegral (factorial k * factorial (n - k))

factorial :: Integral a => a -> a
factorial n = product [1..n]

-- https://en.wikipedia.org/wiki/Binomial_distribution
probSuccess :: Int -> Int -> Rational -> Rational
probSuccess k n p = (n `choose` k) * (p^k) * (1 - p)^(n - k)

probSuccess_ :: Int -> Rational -> Rational
probSuccess_ n p = fromIntegral n * p * (1 - p)^(n - 1)

-- From augustss: https://stackoverflow.com/a/30979717
-- | Convert a 'Rational' to a 'String' using the given number of decimals.
-- If the number of decimals is not given the full precision is showed using (DDD) for repeating digits.
-- E.g., 13.7/3 is shown as \"4.5(6)\".
showRational :: Maybe Int -> Rational -> String
showRational (Just n) r =
    let d = round (abs r * 10^n)
        s = show (d :: Integer)
        s' = replicate (n - length s + 1) '0' ++ s
        (h, f) = splitAt (length s' - n) s'
    in  (if r < 0 then "-" else "") ++ h ++ "." ++ f
-- The length of the repeating digits is related to the totient function of the
-- denominator. This means that the complexity of computing them is at least as
-- bad as factoring, i.e., it quickly becomes infeasible.
showRational Nothing r0 =
    let (i, f) = properFraction (abs r0) :: (Integer, Rational)
        si = if r0 < 0 then "-" ++ show i else show i
        decimals f0 = loop f0 [] ""
        loop x fs ds =
            if x == 0 then
                ds
            else
                case findIndex (x ==) fs of
                    Just j  -> let (l, r) = splitAt j ds in l ++ "(" ++ r ++ ")"
                    Nothing -> let (c, f') = properFraction (10 * x) :: (Integer, Rational) in loop f' (fs ++ [x]) (ds ++ show c)
    in  if f == 0 then si else si ++ "." ++ decimals f
