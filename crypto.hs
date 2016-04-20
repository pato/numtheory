module Crypto where

import Congruences(congInv)
import Data.Char(Char, ord, chr)
import Data.String(String)
import Data.List(sort, group, sortBy)
import Data.Function (on)

caesarShiftEnc :: Int -> String -> String
caesarShiftEnc k plaintext = cyphertext
    where cyphertext = map i2c cypherints
          cypherints = map (\p -> (p + k) `mod` 26) plainints
          plainints  = map c2i plaintext'
          plaintext' = filter (\c -> c /= ' ') plaintext

caesarShiftDec :: Int -> String -> String
caesarShiftDec k cyphertext = plaintext
    where plaintext   = map i2c plainints
          plainints   = map (\p -> (p - k) `mod` 26) cypherints
          cypherints  = map c2i cyphertext'
          cyphertext' = filter (\c -> c /= ' ') cyphertext

affineShiftEnc :: Int -> Int -> String -> String
affineShiftEnc a k plaintext = cyphertext
    where cyphertext = map i2c cypherints
          cypherints = map (\p -> (a * p + k) `mod` 26) plainints
          plainints  = map c2i plaintext'
          plaintext' = filter (\c -> c /= ' ') plaintext

affineShiftDec :: Int -> Int -> String -> String
affineShiftDec a k cyphertext = plaintext
    where plaintext   = map i2c plainints
          plainints   = map (\p -> a' * (p - k) `mod` 26) cypherints
          cypherints  = map c2i cyphertext'
          cyphertext' = filter (\c -> c /= ' ') cyphertext
          a' = congInv a 26

-- Brute force an affine shift encryption
bruteForceAffine :: String -> [(Int, Int, String)]
bruteForceAffine cyphertext = map (\(a,k) -> (a,k,affineShiftDec a k cyphertext)) [(x,y) | x <- [1..26], y <- [1..26]]

-- Get sorted character frequency distributed
freqDist :: String -> [(Char, Int)]
freqDist text = sorted
    where sorted = sortBy (compare `on` snd) freqs
          freqs  = map (\x -> (head x, length x)) . group . sort $ text

-- get numerical difference between two characters
cdiff :: Char -> Char -> Int
cdiff a b = (c2i a - c2i b) `mod` 26

-- convert character to numberical represntation
c2i :: Char -> Int
c2i c = ord c - ord 'a'

-- convert numerical representation to character
i2c :: Int -> Char
i2c i = chr (i + ord 'a')


-- a is the parameters to the mapping fn (have a tuple for > 1)
-- just have an (Int -> Int) and preapply the arguments to the fn (nice)
-- shiftEnc :: a -> (a -> Int -> Int) -> String -> String

-- 8 : "kyvmr clvfw kyvbv pzjjv mvekv ve"
-- 12: "mjmzk cxunm gwiry vcpuw mprrw gmiop msnys ryraz pxmcd wprye yxd"

