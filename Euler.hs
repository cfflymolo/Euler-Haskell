{-# OPTIONS_GHC -Wall #-}
module Euler where

import Helpers

-- Problem 1
-- If we list all the natural numbers below 10 that are multiples
-- of 3 or 5, we get 3, 5, 6, and 9. The sum of these multiples is 23
-- Find the sum of all the multiples of 3 or 5 below 1000
problem1 :: [Int] -> Int
problem1 xs = sum [ x | x <- xs, x `mod` 3 == 0 || x `mod` 5 == 0 ]

-- Each new term in the Fibonacci sequence is generated by adding
-- the previous two terms. By starting with 1 and 2, the first 10
-- terms will be: 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
-- By considering the terms in the Fibonacci sequence whose values
-- do not exceed four million, find the sum of the even valued terms
problem2 :: Int -> Int
problem2 n = sum $ takeWhile (<n) evenFib
    where
        evenFib = [ fibonacci x | x <- [2..], even $ fibonacci x ]

-- The prime factors of 13195 are 5, 7, 13, and 29
-- What is the largest prime factor or the number 600851475143?
problem3 :: Int -> Int
problem3 n = maximum $ primeFactors n
