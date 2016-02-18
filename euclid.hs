euclidGcd :: Integral a => a -> a -> a
euclidGcd a b
    | a > b = euclidGcd b a
    | a == 0 = b
    | otherwise = euclidGcd (b `mod` a) a

euclidLcm :: (Integral a) => a -> a -> a
euclidLcm a b = (a * b) `div` euclidGcd a b
