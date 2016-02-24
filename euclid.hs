-- Get the GCD of two numbers
euclidGcd :: Integral a => a -> a -> a
euclidGcd a b
    | a > b = euclidGcd b a
    | a == 0 = b
    | otherwise = euclidGcd (b `mod` a) a

-- Get the LCM of two numbers
euclidLcm :: (Integral a) => a -> a -> a
euclidLcm a b = (a * b) `div` euclidGcd a b

-- Get the GCD of a list of numbers
multiGcd :: (Integral a) => [a] -> a
multiGcd [a,b]  = euclidGcd a b
multiGcd (x:xs) = euclidGcd x (multiGcd xs)

-- More succint: solve multiple GCD
multiGcd' :: (Integral a) => [a] -> a
multiGcd' xs = foldl euclidGcd 0 xs

-- Get the LCM of a list of numbers
multiLcm :: (Integral a) => [a] -> a
multiLcm [a,b]  = euclidLcm a b
multiLcm (x:xs) = euclidLcm x (multiLcm xs)

-- More succint: solve multiple LCMs
multiLcm' :: (Integral a) => [a] -> a
multiLcm' xs = foldl euclidLcm 1 xs
