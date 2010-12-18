{- |
Module      :  $Header$
Description :  A module for generating prime numbers
Copyright   :  (c) Nicholas Orton
License     :  GPL

Maintainer  :  no | <nick@orton.biz>
Stability   :  provisional
Portability :  portable

Code : https://github.com/nick-orton/project_euler/blob/master/Euler/Primes.hs
various procedures for generating primes
sequence [A000040](http://www.research.att.com/~njas/sequences/A000040)
-}

module Euler.Primes
( isPrime,
  pfacs,
  primes_by_lcd,
  sieve,
  primes_by_wheel,
  lgstPfac
  ) where
import Euler.Divisors (ld)
import List


-- |If the lowest divisor of a number is itself, it is prime
isPrime :: Integer -> Bool
isPrime x = ld x == x

{- |Returns prime factors for some number. The prime factors of 42 are 2, 3,
    and 7 and the prime factors of 74 are 2 and 37
-}
pfacs :: Integer -> [Integer]
pfacs x = nub (pfacs' x [])
pfacs' 1 (xs) = xs
pfacs' n (xs) = pfacs' (div n (ld n)) ((ld n ) : xs)

testpfacs = ((pfacs 42) == [7, 3, 2]) && (pfacs 74 == [37, 2])

-- |the largest prime factor 
lgstPfac x = top $ pfacs x
   where top (y : ys) = y

testlgstPfac = lgstPfac 600851475143 == 6857
-- -----------------------------------------


-- primes_by_wheel is the default prime generation procedure
primes = primes_by_wheel

-- assert the first 25 prime numbers
testprimes = take 25 primes == [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37,
                                41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83,
                                89, 97]

-- | Brute force procedure for generating prime numbers
primes_by_lcd :: [Integer]
primes_by_lcd = filter isPrime [2 ..]

-- |Procedure for generating primes using the Sieve of Eratosthenes.
-- Starting from 2 cross off all of its multiples from the list of integers.
-- The next number you find will be prime.  Cross off all of its multiples,
-- repeat. The numbers left will all be prime
sieve :: [Integer] -> [Integer]
sieve (0 : xs) = sieve xs
sieve (n : xs) = n : sieve (mark xs 1 n)
   where
   mark :: [Integer] -> Integer -> Integer -> [Integer]
   mark (y : ys) k m | k == m = (mark ys 1 m)
                               | otherwise = y : (mark ys (k + 1) m)
primes_from_sieve = sieve [2 ..]

-- |Taken from <http://www.haskell.org/haskellwiki/Prime_numbers>
-- This is more or less the brute force method above, but we don't even consider
-- multiples of the first 10 prime numbers
primes_by_wheel :: [Integer]
primes_by_wheel = known ++ unknown
   where
   1 : p : candidates = roll $ mkWheel known
   known = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
   unknown = p : filter isPrime candidates
   isPrime n = all (not . divides n) $ takeWhile (\p -> p * p <= n) unknown
   divides n p = n `mod` p == 0


data Wheel = Wheel Integer [Integer]
w0 = Wheel 1 [1]

nextSize :: Wheel -> Integer -> Wheel
nextSize (Wheel n rs) p =
  Wheel (p * n) [r' | k <- [0 .. (p - 1)],
                    r <- rs,
                    let r' = n * k + r, r' `mod` p /= 0]

roll :: Wheel -> [Integer]
roll (Wheel n rs) = [n * k + r | k <- [0 .. ], r <- rs]

mkWheel :: [Integer] -> Wheel
mkWheel ds = foldl nextSize w0 ds


-- test the module
testModulePrimes = testpfacs && testlgstPfac && testprimes
