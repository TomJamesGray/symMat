module SymMat.Factorisation (
  factorise,
  primesTo,
  sieve,
  decompose
) where

type Factors = [Int]
type Primes = [Int]

-- | Factorise an integer into it's prime factors
factorise :: Int -> Factors
factorise 1 = [1]
factorise 0 = [0]
factorise n
  | n > 0 = decompose n (2:[3,5..n]) [] --Only do odd numbers after 2 to improve speed
  | otherwise = (decompose (abs n) (primesTo (abs n)) []) ++ [-1]

-- | Used recursively to decompose a given integer to it's prime factors
decompose :: Int -> [Int] -> [Int] -> Factors
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
