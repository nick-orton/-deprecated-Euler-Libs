{- |
Module      :  $Header$
Description :  A module for working with base 10 numbers
Copyright   :  (c) Nicholas Orton
License     :  GPL

Maintainer  :  no | <nick@orton.biz>
Stability   :  provisional
Portability :  portable 

Code : https://github.com/nick-orton/project_euler/blob/master/Lib/Decumal.hs
-}

module Euler.Decimal where
import Char

-- |converts an integer into a list of its digits
toDigits :: Integer -> [Int]
toDigits x = map digitToInt (show x)

testToDigits = toDigits 12345 == [1,2,3,4,5]

-- |converts a list of digits into an integer
fromDigits :: [Int] -> Integer
fromDigits = (\x -> toInteger(foldl addDigit 0 x))
    where addDigit n d = 10 * n + d

testFromDigits = fromDigits [5,3,6,8] == 5368

{- |
converts a decimal number to a list representing a binary number
http://snipplr.com/view/11807/convert-decimal-to-binary-in-haskell/
-}
decToBin x = reverse $ decToBin' x
    where
    decToBin' 0 = []
    decToBin' y = let (a,b) = quotRem y 2 in [b] ++ decToBin' a

testDecToBin = [1,0,1] == decToBin 5

-- |test the module
testDecimalModule = testToDigits && testFromDigits && testDecToBin
