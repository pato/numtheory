import Primes
import Data.Set

phi :: (Integral a, Fractional b) => a -> [b]
phi n = f
    where
        primes = primeFactors n
        primesUnique = toList $ fromList $ primes
        f = Prelude.map (\x -> 1 - 1/x) primesUnique
