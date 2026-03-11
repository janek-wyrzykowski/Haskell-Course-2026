module Solution where
import Distribution.Simple.Utils (xargs)


-- 1. Goldbach Pairs
goldbachPairs :: Int -> [(Int, Int)]
goldbachPairs n
  | n < 4 = []
  | otherwise = [(p, n - p) |
    p <- [2..(floor (fromIntegral n / 2))],
    isPrime p,
    isPrime (n - p)]


-- 2. Coprime Pairs
coprimePairs :: [Int] -> [(Int, Int)]
coprimePairs [] = []
coprimePairs [x] = []
coprimePairs xs = [(x, y) |
  x <- xs,
  y <- xs,
  x < y,
  gcd x y == 1]


-- 3. Sieve of Eratosthenes
sieve :: [Int] -> [Int]
sieve [] = []
sieve [x] = [x]
sieve (x:xs) = x : sieve [y |
  y <- xs,
  mod y x /= 0]

primesTo :: Int -> [Int]
primesTo n = sieve [2..n]

isPrime :: Int -> Bool
isPrime n
  | n < 2 = False
  | otherwise = n `elem` primesTo n


-- 4. Matrix Multiplication
matMul :: [[Int]] -> [[Int]] -> [[Int]]
matMul a b = [[sum [a !! i !! k * b !! k !! j |
      k <- [0..length b - 1]] |
    j <- [0..length (head b) - 1]] |
  i <- [0..length a - 1]]


-- 5. Permutations
-- Unfortunately it is not a perfect solution because it requires comparable list elements (and it doesn't work with non-unique elements)
permutations :: Eq a => Int -> [a] -> [[a]]
permutations k xs
  | k == 0 = [[]]
  | otherwise = concat [fmap (y :)  (permutations (k - 1) (filter (/= y) xs)) |
    y <- xs]


-- 6. Hamming Numbers
-- TODO maybe return to this
-- For some reason it cannot calculate
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x == y = x : merge xs ys
  | x < y = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

hamming :: [Integer]
hamming = merge (merge hamming (fmap (2*) hamming)) (merge (fmap (3*) hamming) (fmap (5*) hamming))


-- 7. Integer Power with Bang Patterns


-- 8. Running Maximum


-- 9. Infinite Prime Stream


-- 10. Strict Accumulation and Space Leaks
