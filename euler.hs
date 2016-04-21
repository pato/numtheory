module Euler where

import Primes
import Data.Set(fromList, toList)

phi :: Int -> Int
phi n = m
    where
        m = foldl (*) 1 $ map (pred) primesUnique
        primesUnique = toList $ fromList $ primes
        primes = primeFactors n
