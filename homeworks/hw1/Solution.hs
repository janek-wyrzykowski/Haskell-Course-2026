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
permutations :: Int -> [a] -> [[a]]
permutations k xs
  | k == 0 = [[]]
  | otherwise = concat [fmap ((xs !! i) :) (permutations (k - 1) (take i xs ++ drop (i+1) xs)) |
    i <- [0..length xs - 1]]


-- 6. Hamming Numbers
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x == y = x : merge xs ys
  | x < y = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

hamming :: [Integer]
hamming = 1 : merge (fmap (2*) hamming) (merge (fmap (3*) hamming) (fmap (5*) hamming))


-- 7. Integer Power with Bang Patterns
power :: Int -> Int -> Int
power a b = go a b 1
  where
    go _ 0 r = r
    go p q !r = go p (q-1) r*p


-- 8. Running Maximum
listMaxSeq :: [Int] -> Int
listMaxSeq (x:xs) = go xs x 
  where
    go [] n = n
    go (y:ys) n = let acc = max y n in seq acc (go ys acc)

listMaxBang :: [Int] -> Int
listMaxBang (x:xs) = go xs x 
  where
    go [] n = n
    go (y:ys) !n = go ys (max y n) 


-- 9. Infinite Prime Stream
primes :: [Int]
primes = sieve [2..]

isPrimeUnbound :: Int -> Bool
isPrimeUnbound n = go n 0
  where
    go a i
      | a > primes !! i = go a (i+1)
      | a == primes !! i = True
      | otherwise = False

-- 10. Strict Accumulation and Space Leaks
meanA :: [Double] -> Double
meanA xs = uncurry (/) (helper xs (0, 0))
  where
    helper [] (a, b) = (a, b)
    helper (x:xs) (a, b) = helper xs (a+x, b+1)

-- Note: Bang patterns reduce to WHNF. A pair in itself is in WHNF, so `!(a, b)` does not force evaluation of `a` nor `b`; to achieve this goal, `(!a, !b)` is required.
meanB :: [Double] -> Double
meanB xs = uncurry (/) (helper xs (0, 0))
  where
    helper [] (a, b) = (a, b)
    helper (x:xs) (!a, !b) = helper xs (a+x, b+1)

meanVarC :: [Double] -> (Double, Double)
meanVarC xs = let (a, b, c) = helper xs (0, 0, 0)
  in (a/c, b/c - (a*a)/(c*c))
  where
    helper [] (a, b, c) = (a, b, c)
    helper (x:xs) (!a, !b, !c) = helper xs (a+x, b+x*x, c+1)