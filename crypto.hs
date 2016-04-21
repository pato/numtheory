module Crypto where

import Congruences(congInv)
import Data.Char(Char, ord, chr)
import Data.String(String)
import Data.List(sort, group, sortBy)
import Data.Function (on)
import Data.Bits(shiftR, testBit)
import Data.Digits(digits, unDigits)
import Data.List.Split(chunksOf)

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

rsaEnc :: Int -> Int -> String -> [Int]
rsaEnc e n plaintext = plainblocks -- cypherblocks
    where cypherblocks = map ( \b -> fmodExp b e n) plainblocks
          plainblocks  = map (unDigits 10) $ chunksOf blockSize plainints
          plainints    = concat $ map c2il plaintext'
          plaintext'   = filter (\c -> c /= ' ') plaintext
          blockSize    = length $ digits 10 n

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

-- convert a character into digit list
c2il :: Char -> [Int]
c2il c
    | length d == 0 = [0,0]
    | length d == 1 = 0 : d
    | otherwise    = d
    where d = digits 10 $ c2i c

-- convert numerical representation to character
i2c :: Int -> Char
i2c i = chr (i + ord 'a')

-- fast modular exponentiation (b^e `mod` m)
fmodExp :: Int -> Int -> Int -> Int
fmodExp b 0 m = 1
fmodExp b e m = t * fmodExp ((b * b) `mod` m) (shiftR e 1) m `mod` m
           where t = if testBit e 0 then b `mod` m else 1

-- a is the parameters to the mapping fn (have a tuple for > 1)
-- just have an (Int -> Int) and preapply the arguments to the fn (nice)
-- shiftEnc :: a -> (a -> Int -> Int) -> String -> String

-- 8 : "kyvmr clvfw kyvbv pzjjv mvekv ve"
-- 12: "mjmzk cxunm gwiry vcpuw mprrw gmiop msnys ryraz pxmcd wprye yxd"

