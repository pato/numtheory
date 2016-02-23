-- Get the GCD of two numbers
euclidGcd :: Integral a => a -> a -> a
euclidGcd a b
    | a > b = euclidGcd b a
    | a == 0 = b
    | otherwise = euclidGcd (b `mod` a) a

-- HW defined algorithm for GCD
hwGCD :: Integral a => a -> a -> a
hwGCD a b
    | a == b           = a
    | even a && even b = 2 * hwGCD (a `div` 2) (b `div` 2)
    | even a && odd b  = hwGCD (a `div` 2) b
    | odd a && odd b   = hwGCD ((max a b) - (min a b)) b

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

-- Prime factorization of number
primeFactors :: (Integral a) => a -> [a]
primeFactors 1 = []
primeFactors n
  | factors == []  = [n]
  | otherwise = factors ++ primeFactors (n `div` (head factors))
  where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n-1]
