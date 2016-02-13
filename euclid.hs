euclidGcd :: Integral a => a -> a -> a
euclidGcd a b
    | a > b = euclidGcd b a
    | a == 0 = b
    | otherwise = euclidGcd (b `mod` a) a
