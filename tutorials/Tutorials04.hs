module Tutorials04 where
import Data.Tuple (swap)
import Data.Monoid (Sum)
import Data.ByteString (count)

-- 1. **Maybe Monad — safe indexing and lookup chains**

--    a. Using `safeTail` and `safeHead` from the lecture, implement `safeIndex :: [a] -> Int -> Maybe a` 
--    that safely returns the element at a given index (0-based), returning `Nothing` if the index is out of bounds.

--    b. Given two association lists (i.e. lists of key-value pairs):
--    ```haskell
--    students :: [(Int, String)]        -- student ID -> name
--    grades   :: [(String, [Int])]      -- name -> list of grades
--    ```
--    write a function `bestGrade :: [(Int, String)] -> [(String, [Int])] -> Int -> Maybe Int` that, 
--    given a student ID, looks up the student's name, then their list of grades, then returns the maximum grade. 
--    If any step fails (unknown ID, unknown name, or empty grade list), the result should be `Nothing`. 

-- naive version
naiveSafeIndex :: Int -> [a] -> Maybe a
naiveSafeIndex _ [] = Nothing
naiveSafeIndex 0 (x:xs) = Just x
naiveSafeIndex n (_:xs) = naiveSafeIndex (n-1) xs

-- desired version
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

safeIndex :: Int -> [a] -> Maybe a
safeIndex 0 list = safeHead list
safeIndex n list = do
  t <- safeTail list
  safeIndex (n-1) t


-- 2. **List Monad and path exploration**

--    Write a function `knights :: (Int, Int) -> [(Int, Int)]` that, given a knight's position on a chessboard,
--    returns a list of all possible knight moves. Then implement a function 
--    `knightPaths :: Int -> (Int, Int) -> (Int, Int) -> [[(Int, Int)]]`
--    that finds all possible paths of length `n` moves from one position to another. Use the list monad and
--    the `>>=` operator to explore all possible paths.

knights :: (Int, Int) -> [(Int, Int)]
knights (x, y) = do
  let halfMoves = [(1, 2), (-1, 2), (-1, -2), (1, -2)]
      moves = halfMoves ++ map swap halfMoves
  (dx, dy) <- moves
  let x' = x + dx
      y' = y + dy
  if (1 <= x') && (x' <= 8) && (1 <= y') && (y' <= 8) then return (x', y') else []

knightPaths :: Int -> (Int, Int) -> (Int, Int) -> [[(Int, Int)]]
knightPaths 0 p1 p2 = if p1 == p2 then return [p1] else []
knightPaths n p1 p2 = do
  step <- knights p1
  path <- knightPaths (n - 1) step p2
  return $ p1:path


-- 3. **Implementing a custom monad**

--    Implement your own monad `Logger a` that stores a value of type `a` together with a log
--    of operations (a list of strings). Define `Functor`, `Applicative`, and `Monad` instances for this type.
--    Write helper functions:
--    - `logMessage :: String -> Logger ()`
--    - `runLogger :: Logger a -> (a, [String])`

data Logger a = Logger {runLogger :: (a, [String])} deriving Show

instance Functor Logger where
  fmap f (Logger (x, logs)) = Logger (f x, logs)

instance Applicative Logger where
  pure x = Logger (x, [])
  liftA2 f (Logger (x, logx)) (Logger (y, logy)) = Logger (f x y, logx ++ logy)

instance Monad Logger where
  return = pure -- optional
  Logger (x, logx) >>= f = let (y, logy) = runLogger (f x) in Logger (y, logx ++ logy)

logMessage :: String -> Logger ()
logMessage message = Logger ((), [message])

--    Then use this monad to implement a function `factorial :: Int -> Logger Int`
--    that computes the factorial and logs each step of the computation.

factorial :: Int -> Logger Int
factorial 0 = do
  logMessage "0th step"
  return 1
factorial n = do
  p <- factorial (n-1)
  logMessage $ show n ++ "th step"
  return $ n*p


-- 4. **Writer Monad for accumulating results**

--    Implement a function `countNodes :: Tree a -> Writer (Sum Int) ()` that counts the nodes in a binary tree
--    using the Writer monad for accumulation. Define the tree type as
--    `data Tree a = Empty | Leaf a | Node a (Tree a) (Tree a)` and use the `tell` function in your solution.

data Writer m a = Writer {runWriter :: (a, m)} deriving (Show, Functor)

instance (Monoid m) => Applicative (Writer m) where
  pure x = Writer (x, mempty)
  liftA2 f (Writer (x, mx)) (Writer (y, my)) = Writer (f x y, mx <> my)

instance (Monoid m) => Monad (Writer m) where
  (Writer (x, mx)) >>= f = let (y, my) = runWriter (f x) in Writer (y, mx <> my)

tell :: (Monoid m) => m -> Writer m ()
tell message  = Writer ((), message)

data Tree a = Empty | Leaf a | Node a (Tree a) (Tree a)

countNodes :: Tree a -> Writer (Sum Int) ()
countNodes Empty = tell 0
countNodes (Leaf x) = tell 1
countNodes (Node x left right) = do
  tell 1
  countNodes left
  countNodes right


-- 5. **State Monad for tracking state**

--    Define a function `runningSum :: [Int] -> [Int]` that, given a list of integers, returns a list of partial sums.
--    For example, for the list `[1, 2, 3, 4]` the result should be `[1, 3, 6, 10]`. Implement this function
--    using the State monad, making use of `get`, `put`, and `runState` or `evalState`.