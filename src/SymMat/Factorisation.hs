module SymMat.Factorisation (
  factorise,
  primesTo,
  sieve,
) where

type Factors = [Int]
type Primes = [Int]

-- | Factorise an integer into it's prime factors
factorise :: Int -> Factors
factorise 1 = [1]
factorise 2 = [2]
factorise n = decompose n (primesTo n) []

-- | Used recursively to decompose a given integer to it's prime factors
decompose :: Int -> Primes -> [Int] -> Factors
decompose 1 _ currF = currF
decompose n possF currF = let
  divisor = possF !! 0
  rem = n `div` (possF !! 0)
  in if (n `mod` divisor == 0) then
    (decompose rem possF (currF ++ [divisor])) else
      (decompose n (drop 1 $ possF) currF)

-- | Generate all primes that are all less than or equal to n
primesTo :: Int -> Primes
primesTo n
  | n > 1 = let
    ns = [2..n]
    zeroNP = filter (>0) (sieve ns 2) -- Remove the -1's
    in zeroNP
  | otherwise = []

-- | Recursive implementation of the sieve of Eratosthenes, to 'remove'
-- a number it sets it to -1
sieve :: [Int] -> Int -> [Int]
sieve ns step
  | fromIntegral step > (fromIntegral (length ns) + 1) ** 0.5 = ns
  | otherwise = let
      sieved = foldr (\x acc -> if (x > step) && (x `mod` step == 0) then -1:acc else x:acc) [] ns
      nextStep = head $ filter (>step) sieved
      in sieve sieved nextStep
