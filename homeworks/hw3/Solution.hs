module Solution where

import Control.Monad.Writer (Writer, tell)
import Data.Map (Map, (!?))

-- 1. **Maze navigation**

--    A maze is represented as a map from positions to their neighbours in each direction:
--    ```haskell
--    type Pos = (Int, Int)
--    data Dir = N | S | E | W deriving (Eq, Ord, Show)
--    type Maze = Map Pos (Map Dir Pos)
--    ```
--    A position may not have a neighbour in every direction (walls).

type Pos = (Int, Int)

data Dir = N | S | E | W deriving (Eq, Ord, Show)

type Maze = Map Pos (Map Dir Pos)

--    (a) Write a function
--    ```haskell
--    move :: Maze -> Pos -> Dir -> Maybe Pos
--    ```
--    that attempts a single move in the given direction, returning `Nothing` if blocked by a wall.

move :: Maze -> Pos -> Dir -> Maybe Pos
move maze pos dir = do
  options <- maze !? pos
  options !? dir

--    (b) Write a function
--    ```haskell
--    followPath :: Maze -> Pos -> [Dir] -> Maybe Pos
--    ```
--    that follows a sequence of directions from a starting position, short-circuiting to `Nothing`
--    as soon as any step is blocked. Use the `Maybe` monad — do not pattern-match on `Nothing` manually.

followPath :: Maze -> Pos -> [Dir] -> Maybe Pos
followPath maze pos (dir : dirs) = do
  pos' <- move maze pos dir
  followPath maze pos' dirs

--    (c) Write a function
--    ```haskell
--    safePath :: Maze -> Pos -> [Dir] -> Maybe [Pos]
--    ```
--    that returns the full trace of positions visited (including the start), or `Nothing` if the
--    path is blocked at any point.

safePath :: Maze -> Pos -> [Dir] -> Maybe [Pos]
safePath maze pos (dir : dirs) = do
  pos' <- move maze pos dir
  path <- safePath maze pos' dirs
  return $ pos : path

-- 2. **Decoding a message**

--    A substitution cipher maps each character to another. Given a partial decryption key
--    (not every character may be known yet):
--    ```haskell
--    type Key = Map Char Char
--    ```

--    write a function
--    ```haskell
--    decrypt :: Key -> String -> Maybe String
--    ```
--    that decodes the entire string, returning `Nothing` if any character in the input is missing
--    from the key. Use `traverse` with the `Maybe` monad.

type Key = Map Char Char

decrypt :: Key -> String -> Maybe String
decrypt key (str : strs) = do
  decrypted <- key !? str
  decryptedList <- decrypt key strs
  return $ decrypted : decryptedList

--    Then write
--    ```haskell
--    decryptWords :: Key -> [String] -> Maybe [String]
--    ```
--    that decrypts a list of words, failing if any single word cannot be fully decoded.

decryptWords :: Key -> [String] -> Maybe [String]
decryptWords key (word : words) = do
  decrypted <- decrypt key word
  decryptedList <- decryptWords key words
  return $ decrypted : decryptedList

-- 3. **Seating arrangements**

--    You are organising a dinner and must assign guests to seats around a table.
--    Some pairs of guests have conflicts and must not sit next to each other.
--    ```haskell
--    type Guest = String
--    type Conflict = (Guest, Guest)
--    ```
--    Write a function
--    ```haskell
--    seatings :: [Guest] -> [Conflict] -> [[Guest]]
--    ```
--    that returns all valid permutations of the guest list such that no two conflicting guests
--    are adjacent (the table is round, so the last and first guests are also neighbours).
--    Use the list monad to generate permutations and `guard` to filter out invalid ones.

type Guest = String

type Conflict = (Guest, Guest)

seatings :: [Guest] -> [Conflict] -> [[Guest]]
seatings = undefined

-- 4. **Result monad with warnings**

--    Define a type
--    ```haskell
--    data Result a = Failure String | Success a [String]
--    ```
--    where `Failure msg` represents a computation that failed with an error message, and
--    `Success val warnings` represents a successful computation carrying a value together with
--    a list of accumulated warning messages.

data Result a = Failure String | Success a [String] deriving (Show)

--    (a) Implement `Functor`, `Applicative`, and `Monad` instances for `Result`.
--    Warnings should be accumulated (concatenated) when sequencing computations.

instance Functor Result where
  fmap _ (Failure e) = Failure e
  fmap fun (Success x w) = Success (fun x) w

instance Applicative Result where
  pure x = Success x []
  liftA2 _ (Failure e) _ = Failure e
  liftA2 _ _ (Failure e) = Failure e
  liftA2 fun (Success x wx) (Success y wy) = Success (fun x y) (wx ++ wy)

instance Monad Result where
  Failure e >>= fun = Failure e
  Success x wx >>= fun = let Success y wy = fun x in Success y (wx ++ wy)

--    (b) Write helper functions:
--    ```haskell
--    warn    :: String -> Result ()
--    failure :: String -> Result a
--    ```

warn :: String -> Result ()
warn w = Success () [w]

failure :: String -> Result a
failure = Failure

--    (c) Use the `Result` monad to implement a function
--    ```haskell
--    validateAge :: Int -> Result Int
--    ```
--    that fails if the age is negative, warns if the age is above 150, and otherwise succeeds with the age.

validateAge :: Int -> Result Int
validateAge n
  | n < 0 = failure "Negative age"
  | n > 150 = warn ("Age " ++ show n ++ " over 150") >> return n
  | otherwise = return n

--    Then implement
--    ```haskell
--    validateAges :: [Int] -> Result [Int]
--    ```
--    that validates a list of ages, accumulating all warnings. Use `mapM`.

validateAges :: [Int] -> Result [Int]
validateAges = mapM validateAge

-- 5. **Evaluator with simplification log**

--    Given the expression type:
--    ```haskell
--    data Expr = Lit Int | Add Expr Expr | Mul Expr Expr | Neg Expr
--    ```
--    write a function
--    ```haskell
--    simplify :: Expr -> Writer [String] Expr
--    ```
--    that applies algebraic simplification rules and logs each rule applied. The rules should include
--    at least:
--    - `Add (Lit 0) e` or `Add e (Lit 0)` simplifies to `e` (additive identity)
--    - `Mul (Lit 1) e` or `Mul e (Lit 1)` simplifies to `e` (multiplicative identity)
--    - `Mul (Lit 0) _` or `Mul _ (Lit 0)` simplifies to `Lit 0` (zero absorption)
--    - `Neg (Neg e)` simplifies to `e` (double negation)
--    - `Add (Lit a) (Lit b)` simplifies to `Lit (a+b)` (constant folding)
--    - `Mul (Lit a) (Lit b)` simplifies to `Lit (a*b)` (constant folding)

--    Each time a rule fires, log a message like `"Add identity: 0 + e -> e"`.
--    Apply simplification recursively (bottom-up: simplify subtrees first, then the root).

data Expr = Lit Int | Add Expr Expr | Mul Expr Expr | Neg Expr deriving (Show, Eq)

simplify :: Expr -> Writer [String] Expr
simplify (Lit x) = return $ Lit x
simplify (Add x y) = do
  x' <- simplify x
  y' <- simplify y
  if x' == Lit 0 then tell ["0 + e -> e"] >> return y'
  else if y' == Lit 0 then tell ["e + 0 -> e"] >> return x'
  else return $ Add x' y'
simplify (Mul x y) = do
  x' <- simplify x
  y' <- simplify y
  if x' == Lit 0 then tell ["0 * e -> 0"] >> return (Lit 0)
  else if y' == Lit 0 then tell ["e * 0 -> 0"] >> return (Lit 0)
  else if x' == Lit 1 then tell ["1 * e -> e"] >> return y'
  else if y' == Lit 1 then tell ["e * 1 -> e"] >> return x'
  else return $ Mul x' y'
simplify (Neg x) = do
  x' <- simplify x
  if isNeg x' then tell ["- (- e) = e"] >> return (dropNeg x')
  else return $ Neg x'
  where
    isNeg (Neg e) = True
    isNeg _ = False
    dropNeg (Neg e) = e

-- 6. **ZipList — an Applicative that is not a Monad**

--    `ZipList` applies functions to elements *positionally* (by zipping), rather than generating all combinations like the standard list `Applicative`:
--    ```haskell
--    newtype ZipList a = ZipList { getZipList :: [a] } deriving (Show)
--    ```

newtype ZipList a = ZipList { getZipList :: [a] } deriving (Show)

--    (a) Implement the `Functor` and `Applicative` instances for `ZipList`:
--    ```haskell
--    instance Functor ZipList where
--      fmap = ???

--    instance Applicative ZipList where
--      pure  = ???
--      (<*>) = ???
--    ```
--    The idea behind `ZipList` is that `(<*>)` pairs up elements at matching positions (first with first, second with second, etc.). For `pure`, you need a `ZipList` that acts as a neutral element — it should work with any list regardless of length.

instance Functor ZipList where
  fmap f l = ZipList $ map f $ getZipList l

instance Applicative ZipList where
  pure x = ZipList infList
    where
      infList = x : infList
  ZipList [] <*> _ = ZipList []
  _ <*> ZipList [] = ZipList []
  ZipList (f:fs) <*> ZipList (x:xs) = let ZipList ys = ZipList fs <*> ZipList xs in ZipList (f x : ys)

--    (b) Verify that your instance satisfies the applicative laws by testing:
--    ```haskell
--    pure id <*> ZipList [1,2,3]                          -- should be ZipList [1,2,3]
--    pure (+) <*> ZipList [1,2,3] <*> ZipList [10,20,30]  -- should be ZipList [11,22,33]
--    ```

--    (c) Explain (in a comment) why `ZipList` cannot have a lawful `Monad` instance. Specifically, what goes wrong when you try to define `>>=`? Consider what happens when the function passed to `>>=` returns lists of different lengths.

-- There is no sensible way to combine `ZipList`s returned by the function passed to `>>=` into one `ZipList`. The main assumption about `ZipList` type is that positions of elements matter. As a result, simple concatenation wouldn't work (because elements would change positions) and zipping them is impossible because of different lengths (and lack of a universal folding mechanism).