module Helpers where

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x - 1) + fibonacci (x - 2)

factors :: Int -> [Int]
factors 0 = []
factors n = takeWhile (<n) [ x | x <- [2..], n `mod` x == 0 ]

isPrime :: Int -> Bool
isPrime n = (length $ factors n) == 0

primeFactors :: Int -> [Int]
primeFactors n = filter (isPrime) $ factors n
