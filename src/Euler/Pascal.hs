{- |
Module      :  $Header$
Description :  Pascal's triangle
Copyright   :  (c) Nicholas Orton
License     :  GPL

Maintainer  :  no | <nick@orton.biz>
Stability   :  provisional
Portability :  portable

Code : https://github.com/nick-orton/project_euler/blob/master/Lib/Pascal.hs
-}

module Euler.Pascal where

-- |Generate the next row of the triangle given a list
-- representing the current row.  This is done by pushing
-- each number down and to the left.
nextRow :: [Integer] -> [Integer]
nextRow ts = nextRow' ts [0]
            where
            nextRow' [] bs = bs
            nextRow' (t : ts) (b : bs) = nextRow' ts (t : t + b : bs)

testNextRow = (nextRow [1, 2, 1]) == [1, 3, 3, 1]

-- |get the nth row of a pascal's triangle
nthRowTriangle :: Integer -> [Integer]
nthRowTriangle n = nth n [1]
                   where
                   nth 0 r = r
                   nth m r = nth (pred m) (nextRow r)

testNthRowTriangle = nthRowTriangle 3 == [1, 3, 3, 1]
