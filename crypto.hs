module Crypto where

import Congruences
import Data.Char
import Data.String

caesarShiftEnc :: Int -> String -> String
caesarShiftEnc k plaintext = cyphertext
    where cyphertext = map i2c cypherints
          cypherints = map (\p -> (p + k) `mod` 26) plainints
          plainints  = map c2i plaintext

caesarShiftDec :: Int -> String -> String
caesarShiftDec k cyphertext = plaintext
    where plaintext  = map i2c plainints
          plainints  = map (\p -> (p - k) `mod` 26) cypherints
          cypherints = map c2i cyphertext

affineShiftEnc :: Int -> Int -> String -> String
affineShiftEnc a k plaintext = cyphertext
    where cyphertext = map i2c cypherints
          cypherints = map (\p -> (a * p + k) `mod` 26) plainints
          plainints  = map c2i plaintext

affineShiftDec :: Int -> Int -> String -> String
affineShiftDec a k cyphertext = plaintext
    where plaintext  = map i2c plainints
          plainints  = map (\p -> a' * (p - k) `mod` 26) cypherints
          cypherints = map c2i cyphertext
          a' = congInv a 26


-- encodeByChar (Char -> Char) -> String -> String

c2i :: Char -> Int
c2i c = ord c - ord 'a'

i2c :: Int -> Char
i2c i = chr (i + ord 'a')
