module Crypto where

import Data.Char
import Data.String

caesarShiftEnc :: Int -> String -> String
caesarShiftEnc k plaintext = cyphertext
    where cyphertext = map i2c cypherints
          cypherints = map (\p -> (p + k) `mod` 26) plainints
          plainints  = map c2i plaintext

affineShiftEnc :: Int -> Int -> String -> String
affineShiftEnc a k plaintext = cyphertext
    where cyphertext = map i2c cypherints
          cypherints = map (\p -> (a * p + k) `mod` 26) plainints
          plainints  = map c2i plaintext


c2i :: Char -> Int
c2i c = ord c - ord 'a'

i2c :: Int -> Char
i2c i = chr (i + ord 'a')
