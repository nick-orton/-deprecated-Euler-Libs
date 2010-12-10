{- |
Module      :  $Header$
Description :  A module for working with series
Copyright   :  (c) Nicholas Orton
License     :  GPL

Maintainer  :  no | <nick@orton.biz>
Stability   :  provisional
Portability :  portable

Code : https://github.com/nick-orton/project_euler/blob/master/Lib/Numz.lhs
-}

module Euler.Numz where


-- |The set of Natural Numbers ℕ is the set of all counting integers:
-- {1,2,3,...}
nats :: [Integer]
nats = [1 .. ]

-- |A number is natural if we drop the fractional component without a change
-- in value
isNat :: (RealFrac a) => a -> Bool
isNat x = x == fromInteger (floor x)
testisNat = isNat 25 && not (isNat 25.1)

-- |The set of Square Numbers is the set of all integers who's square root
-- is a whole number
isSqr :: (Floating a, RealFrac a) => a -> Bool
isSqr x = isNat (sqrt x)
testisSqr = isSqr 25 && not (isSqr 10)

-- |The factorial of a non-negative integer n, denoted by n!, is the product
-- of all positive integers less than or equal to n
(!) :: (Num t) => t -> t
(!) 0 = 1
(!) 1 = 1
(!) n = n * (!) (n - 1)

-- for example:  5! = 5 x 4 x 3 x 2 x 1 = 120
testFactorial = ((!) 5 == 120) && ((!) 0 == 1)

-- |Fibinacci Series: <http://oeis.org/A000045>
fib :: [Integer] -> [Integer]
fib (y : z : xs) = y + z : fib (y + z : y : z : xs)

fibs = fib [0, 1]
testfib = take 7 fibs == [1, 1, 2, 3, 5, 8, 13]

-- |Triangle Number Series: <http://oeis.org/A000217>
--
-- The sequence of triangle numbers is generated by adding the natural numbers.
-- So the 7^(th) triangle number would be 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28.
--
-- Tn=n(n+1)/2   1, 3, 6, 10, 15, ...
tris :: [Integer]
tris = map (\x -> div (x * (x + 1)) 2) nats

-- The first ten terms would be:
-- 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
testtris = (take 10 tris) == [1, 3, 6, 10, 15, 21, 28, 36, 45, 55]

-- |Pentagonal Number Series: <http://oeis.org/A000326>
--
-- Pn=n(3n+1)/2   1, 5, 12, 22, 35, ...
pents :: [Integer]
pents = map (\x -> div (x * (3 * x - 1)) 2) nats

testPents = take 5 pents == [1, 5, 12, 22, 35]

-- |Hexagonal Number Series: <http://oeis.org/A000384>
--
-- Hn=n(2n+1)   1, 6, 15, 28, 45, ...
hexes :: [Integer]
hexes = map (\x -> x * (2 * x - 1)) nats

testHexes = take 5 hexes == [1, 6, 15, 28, 45]

-- module test
testNumz = testisNat && testisSqr && testfib && testFactorial
           && testPents && testHexes