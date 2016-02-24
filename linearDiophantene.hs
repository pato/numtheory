-- Get a solution to linear diophantene equation
solve :: (Integral a) => a -> a -> a -> (a,a)
solve a b c = (c*s, c*t)
  where (gcd, s, t) = eGcd a b


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


-- Get the form of all solutions to diophantene equatio
-- solve a b c (x,y) = (Coefficient a, Constant c, Grounding g) => a -> a -> c -> (g,g) -> ((c,a),(c,a))
