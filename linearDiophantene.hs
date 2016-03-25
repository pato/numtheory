import Data.List (sortOn)

-- Check if solutions exist to a linear diophantene equation
solutionsExist :: (Integral a) => a -> a -> a -> Bool
solutionsExist a b c = c `mod` (gcd a b) == 0

-- Solve a linear congruence
solveCongruence :: (Integral a) => a -> a-> a-> a
solveCongruence a b m = (fst (solve a m b)) `mod` m

-- Calculate a congruence inverse
congInv :: (Integral a) => a -> a-> a
congInv n m = solveCongruence n 1 m

-- Solve system of equations using Chinese Remainder Theorem
solveCRT :: (Integral a) => [(a,a)] -> a
solveCRT xs =  x `mod` m
    where x = foldl (+) 0 (map f xs)
          ns = map (\(c,n) -> n) xs
          m = foldl (*) 1 ns
          f = \(c,n) -> c * (congInv (getM ns n) n) * (getM ns n)

-- Given a list of numbers and a number, return the product of every element
-- in the first list except for the specified number
getM :: (Integral a) => [a] -> a -> a
getM xs n = foldl (*) 1 (filter (\x -> x /= n) xs)


-- Get a solution to linear diophantene equation
-- or (0,0) if there are no solutions
solve :: (Integral a) => a -> a -> a -> (a,a)
solve a b c = (mult * s, mult * t)
  where (gcd, s, t) = eGcd a b
        mult = c `div` gcd

-- Get the infinite set of all solutions to diophantene linear equation
-- Note: Undefined if there are no solutions
solveAll :: (Integral a) => a -> a -> a-> [(a,a)]
solveAll a b c = [ (x0 + (b `div` gcd) * t, y0 - (a `div` gcd) * t) | t <- ints]
    where (gcd, s, t) = eGcd a b
          mult =  c `div` gcd
          x0 = s * mult
          y0 = t * mult

-- Extended GCD helper function which takes
-- a tuple representing the last two entries of calculation
-- which is (r', s', t', r, s, t) where the primed variables
-- are the last values of r,s,t
eGcd' :: (Integral a) => (a,a,a,a,a,a) -> (a,a,a,a,a,a)
eGcd' (r', s', t',r, s, t)
  | r == 0 = (r', s', t', 0, 0, 0)
  | otherwise = eGcd' (r, s, t, r' - quot * r, s' - quot * s, t' - quot*t)
    where quot = r' `div` r

-- Calculate the extended GCD of two numbers and return (gcd, s, t) where
-- s and t are the Bezout coefficients (ie: x and y in ax + by = 1)
eGcd :: (Integral a) => a -> a -> (a, a, a)
eGcd a b = (r, s, t)
  where (r,s,t,_,_,_) = eGcd' (a, 1, 0, b, 0, 1)


-- Set of all integers (diagonalized)
ints :: Integral a => [a]
ints = concatMap (\x -> [x, negate x]) [1..]

-- Homework Problem 5.4
positiveSolutions = filter ((>1) . snd) $ filter ((>0) . fst) $ (solveAll 18 33 549)
-- Get the 3 solutions that have both positive values and sort them based on their sum
sortedSolutions =  sortOn (\(a,b) -> a + b) $ take 3 positiveSolutions
-- Get the smallest
smallest = head sortedSolutions
-- Number of oranges + grapefruit
fruits = (o + g) where (o,g) = smallest
