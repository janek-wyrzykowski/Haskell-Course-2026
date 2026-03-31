module Solution where

-- Throughout this homework we use the following sequence type:
-- ```haskell
-- data Sequence a = Empty | Single a | Append (Sequence a) (Sequence a)
-- ```
-- A `Sequence a` represents a sequence of values of type `a`. `Empty` is the empty sequence, `Single x` is a one-element sequence, and `Append l r` is the concatenation of two subsequences.

data Sequence a = Empty | Single a | Append (Sequence a) (Sequence a)


-- 1. **Functor for Sequence**

--    Write a `Functor` instance for `Sequence`:
--    ```haskell
--    instance Functor Sequence where
--        fmap :: (a -> b) -> Sequence a -> Sequence b
--    ```

instance Functor Sequence where
  fmap :: (a -> b) -> Sequence a -> Sequence b
  fmap f Empty = Empty
  fmap f (Single x) = Single (f x)
  fmap f (Append seq1 seq2) = Append (fmap f seq1) (fmap f seq2)


-- 2. **Foldable for Sequence**

--    Write a `Foldable` instance for `Sequence` by implementing `foldMap`:
--    ```haskell
--    instance Foldable Sequence where
--        foldMap :: Monoid m => (a -> m) -> Sequence a -> m
--    ```
--    Your traversal order should be *left-to-right*: the left subsequence of an `Append` is processed before the right. Once the instance is in place, use library functions from `Foldable` to define:
--    - `seqToList :: Sequence a -> [a]` — returns all elements in left-to-right order
--    - `seqLength :: Sequence a -> Int` — returns the number of elements

instance Foldable Sequence where
  foldMap :: Monoid m => (a -> m) -> Sequence a -> m
  foldMap f Empty = mempty
  foldMap f (Single x) = f x
  foldMap f (Append seq1 seq2) = foldMap f seq1 <> foldMap f seq2

seqToList :: Sequence a -> [a]
seqToList = foldMap (:[])

seqLength :: Sequence a -> Int
seqLength = foldl (\seed _ -> seed + 1) 0


-- 3. **Semigroup and Monoid for Sequence**

--    Define `Semigroup` and `Monoid` instances for `Sequence a`:
--    ```haskell
--    instance Semigroup (Sequence a) where
--        (<>) :: Sequence a -> Sequence a -> Sequence a

--    instance Monoid (Sequence a) where
--        mempty :: Sequence a
--    ```

instance Semigroup (Sequence a) where
  (<>) :: Sequence a -> Sequence a -> Sequence a
  seq1 <> seq2 = Append seq1 seq2

instance Monoid (Sequence a) where
  mempty :: Sequence a
  mempty = Empty


-- 4. **Tail Recursion and Sequence Search**

--    Write a function
--    ```haskell
--    tailElem :: Eq a => a -> Sequence a -> Bool
--    ```
--    that searches for an element in a `Sequence` using tail recursion with an explicit stack (a list of `Sequence a` values) to manage subsequences still to be inspected.

tailElem :: Eq a => a -> Sequence a -> Bool
tailElem x = go []
  where
    go [] Empty = False
    go (s:stack) Empty = go stack s
    go stack (Single y) = (x == y) || go stack Empty
    go stack (Append seq1 seq2) = go (seq2:stack) seq1


-- 5. **Tail Recursion and Sequence Flatten**

--    Write a tail-recursive function
--    ```haskell
--    tailToList :: Sequence a -> [a]
--    ```
--    that converts a `Sequence a` to a list `[a]` in left-to-right order. 

tailToList :: Sequence a -> [a]
tailToList = go [] []
  where
    go [] acc Empty = acc
    go (s:stack) acc Empty = go stack acc s
    go stack acc (Single x) = go stack (x:acc) Empty
    go stack acc (Append seq1 seq2) = go (seq2:stack) acc seq1


-- 6. **Tail Recursion and Reverse Polish Notation**

--    A *Reverse Polish Notation* (RPN) expression is a sequence of tokens:
--    ```haskell
--    data Token = TNum Int | TAdd | TSub | TMul | TDiv
--    ```
--    Evaluation uses a stack: numbers are pushed; operators pop two values, apply the operation, and push the result back.

--    Write a tail-recursive function
--    ```haskell
--    tailRPN :: [Token] -> Maybe Int
--    ```
--    that processes the token list using a list as the operand stack accumulator. Return `Nothing` for malformed expressions (too few operands, tokens remaining after the final result) or division by zero.

data Token = TNum Int | TAdd | TSub | TMul | TDiv

tailRPN :: [Token] -> Maybe Int
tailRPN = go []
  where
    go stack (TNum x : tokens) = go (TNum x : stack) tokens
    go (TNum x : TNum y : stack) (TAdd : tokens) = go (TNum (x + y) : stack) tokens
    go (TNum x : TNum y : stack) (TSub : tokens) = go (TNum (x - y) : stack) tokens
    go (TNum x : TNum y : stack) (TMul : tokens) = go (TNum (x * y) : stack) tokens
    go (TNum _ : TNum 0 : _) (TDiv : _) = Nothing
    go (TNum x : TNum y : stack) (TDiv : tokens) = go (TNum (x `div` y) : stack) tokens
    go [TNum x] [] = Just x
    go _ _ = Nothing  -- this definition should catch all malformed expressions


-- 7. **Expressing functions via `foldr` and `foldl`**

--     Without using explicit recursion, implement the following functions using `foldr` and/or `foldl`:

--     (a) `myReverse :: [a] -> [a]` — reverses a list. Use `foldl`. 

myReverse :: [a] -> [a]
myReverse = foldl (\seed x -> x : seed) []

--     (b) `myTakeWhile :: (a -> Bool) -> [a] -> [a]` — returns the longest prefix of elements satisfying the predicate 
--     (e.g. `myTakeWhile even [2,4,3,6] = [2,4]`). Use `foldr`. 

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile predicate = foldr (\x seed -> if predicate x then x:seed else []) []

--     (c) `decimal :: [Int] -> Int` — interprets a list of digits as a decimal number, e.g. `decimal [1,2,3] = 123`.

decimal :: [Int] -> Int
decimal = foldl (\seed x -> seed*10 + x) 0


-- 8. **Run-length encoding via folds**

--    *Run-length encoding* compresses a list by replacing consecutive runs of the same element with a pair of the element and its count.

--    (a) Implement `encode :: Eq a => [a] -> [(a, Int)]` using `foldr`. For example:
--    ```haskell
--    encode "aaabccca" = [('a',3),('b',1),('c',3),('a',1)]
--    ```

encode :: Eq a => [a] -> [(a, Int)]
encode = foldr encodeFunction []
  where
    encodeFunction x [] = [(x, 1)]
    encodeFunction x ((y, n) : seed) = if x == y then (y, n+1) : seed else (x, 1) : (y, n) : seed

--    (b) Implement `decode :: [(a, Int)] -> [a]` using `foldr` (and `replicate`). For example:
--    ```haskell
--    decode [('a',3),('b',1),('c',3),('a',1)] = "aaabccca"
--    ```

decode :: [(a, Int)] -> [a]
decode = foldr (\(x, n) seed -> replicate n x ++ seed) []
