{- |
Module      :  $Header$
Description :  A module for ...
Copyright   :  (c) Nicholas Orton
License     :  GPL

Maintainer  :  no | <nick@orton.biz>
Stability   :  provisional
Portability :  portable 

Code : https://github.com/nick-orton/project_euler/blob/master/Euler/${Module}.lhs
-}

module Euler.Divisors where
import List

-- |Simple test to see if the first argument is a divisor of the second.
-- we have syntactic sugar infix operator for this: <. 
isFacOf :: Integer -> Integer -> Bool
isFacOf k n = rem n k == 0

-- |Syntactic sugar for isFacOf
(<.) :: Integer -> Integer -> Bool
(<.) x y = isFacOf x y

-- 1 divides 4, 6 divides 42 and 13 is not a divisor of 55
testisFacOf = (1 <. 4) && (6 <. 42) && not(13 <. 55)

-- |
-- All divisors of of a number.  We find the first half of the list by examining 
-- all natural numbers between 1 and the square root of n.  We get the second half 
-- by finding the quotients of n and the first half of the list. 
divisors :: Integer -> [Integer]
divisors x = nub((smDivisors) ++ reverse (lgDivisors (smDivisors)))
   where 
   smDivisors = [y| y <- [1..(floor (sqrt (fromInteger x)))], (y <. x)]
   lgDivisors [] = []
   lgDivisors (d:ds) = (quot x d) : lgDivisors ds 

testdivisors = ((divisors 60) == [1,2,3,4,5,6,10,12,15,20,30,60])
               && ((divisors 4) == [1,2,4])

-- |The proper divisors of a number are all the divisors of that number excluding
-- itself
properDivs :: Integer -> [Integer]
properDivs n = delete n (divisors n)
testproperDivs = properDivs 60 == [1,2,3,4,5,6,10,12,15,20,30]

-- |Given an integer, the following returns the lowest non-trivial divisor.
-- This will, however, return the input if it is a prime number.
-- This takes advantage of the fact that if a number is not prime, its lowest 
-- divisor is less than the square root of that number
ld ::Integer -> Integer
ld x = ld' 2
   where
   ld' :: Integer -> Integer
   ld' c | c <. x = c
         | c^2 > x = x
         | otherwise = ld' (c+1)
            
-- The lowest non-trivial divisor of 9 is 3 and 4 is 2
testld = (ld 9 == 3) && (ld 4 == 2)

sumDivs n = sum(properDivs n)

-- Perfect Numers
-----------------

-- |A perfect number is a number for which the sum of its proper divisors is 
-- exactly equal to the number. 
--
-- For example, the sum of the proper divisors of 28 
-- would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.
isPerfect :: Integer -> Bool
isPerfect n = sumDivs n == n

testPerfect = isPerfect 28


-- Abundent and Deficient Numbers
-- ------------------------------

-- |n is deficient if the sum of its proper divisors is less than itself
isDeficient :: Integer -> Bool
isDeficient n = sumDivs n < n

testIsDeficient = isDeficient 9

-- |n  is abundant if the sum of its proper divisors exceeds itself
isAbundant :: Integer -> Bool
isAbundant n = sumDivs n > n

testIsAbundant = isAbundant 12

-- Amicable Numbers
-------------------
--
-- |Amicable numbers  are two different numbers related such that 
-- that the sum of the proper divisors of one of the numbers is equal to the 
-- other.  A pair of amicable numbers constitutes an aliquot sequence of 
-- period 2. 
--
-- <http://en.wikipedia.org/wiki/Amicable_number>
hasAmicablePair :: Integer -> Bool
hasAmicablePair x = x == sumDivs (sumDivs x) && (x /= sumDivs x)

-- For example, the smallest pair of amicable numbers is (220, 284); 
-- for the proper divisors of 220 are [1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110], 
-- of which the sum is 284; and the proper divisors of 284 are 
-- [1, 2, 4, 71, and 142], of which the sum is 220.
testhasAmicablePair = hasAmicablePair 220  && hasAmicablePair 284

testModuleDivisors = testhasAmicablePair 
               && testproperDivs 
               && testld 
               && testdivisors 
               && testisFacOf 
               && testPerfect 
               && testIsDeficient
               && testIsAbundant
