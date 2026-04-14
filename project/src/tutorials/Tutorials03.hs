module Tutorials03 where

-- 1. **Binary Search Tree Dictionary with Balancing**

--    Define an algebraic data type representing a binary search tree (BST) that acts as a dictionary mapping keys to values. Implement the following operations:
--    - *Insert*: Add a key-value pair to the tree.
--    - *Lookup*: Retrieve the value associated with a given key.
--    - *Delete*: Remove a key (and its associated value) from the tree.
--    - *Update*: Modify the value associated with an existing key.
--    - *Balance*: Implement a balancing procedure (e.g. AVL or red-black tree) so that the tree remains balanced after insertions and deletions.

data BST a = Empty | Node a (BST a) (BST a) deriving (Show, Eq)

insertBST :: Ord key => key -> value -> BST (key, value) -> BST (key, value)
insertBST k v Empty = Node (k, v) Empty Empty
insertBST k v (Node node@(k', v') left right)
  | k < k' = Node node (insertBST k v left) right
  | k > k' = Node node left (insertBST k v right)
  | otherwise = Node (k, v) left right

lookupBST :: Ord key => key -> BST (key, value) -> Maybe value
lookupBST k Empty = Nothing
lookupBST k (Node (k', v') left right)
  | k == k' = Just v'
  | k < k' = lookupBST k left
  | k > k' = lookupBST k right



-- 2. **Expression Interpreter with Differentiation and Simplification**

--    Define an algebraic data type representing arithmetic expressions, including variables, numeric constants, addition, multiplication, and exponentiation. Write functions that:
--    - *Evaluate*: Compute the numeric value of an expression given a mapping from variable names to numbers.
--    - *Differentiate*: Symbolically differentiate an expression with respect to a given variable.
--    - *Simplify*: Reduce an expression to a simpler form by applying algebraic identities (e.g. eliminating zero terms, collapsing constant subexpressions, combining like terms).



-- 3. **Graph Representation and Algorithms**

--    Define an algebraic data type representing an undirected graph whose vertices can store arbitrary data. Write functions that:
--    - *Depth-First Search (DFS)*: Traverse the graph starting from a given vertex, returning the vertices visited in order.
--    - *Cycle Detection*: Determine whether the graph contains a cycle.
--    - *Path Finding*: Find a path between two vertices, returning `Nothing` if no path exists.



-- 4. **Rose Trees**

-- A *rose tree* (also called a *multi-way tree* or *ordered tree*) is a generalisation of a binary tree in which each node may have any number of children — zero, one, two, or more — and the order of those children is significant. The name comes from the resemblance of a fully-branched tree to a rose, and was popularised in functional programming by Lambert Meertens.

-- ```haskell
-- data RoseTree a = RoseNode a [RoseTree a]
-- ```
-- For example, `RoseNode 1 [RoseNode 2 [], RoseNode 3 [RoseNode 4 []]]` represents a tree with root 1, two children 2 and 3, and 4 as a child of 3.

data RoseTree a = RoseNode a [RoseTree a]

-- a. **Show instance for RoseTree**

--    Write a `Show` instance for `RoseTree a` (assuming `Show a`) that displays a rose tree in a readable nested form. For example, the tree above might display as:
--    ```
--    1 [2 [], 3 [4 []]]
--    ```
--    Do not use `deriving Show` — write the instance by hand.

instance Show a => Show (RoseTree a) where
  show (RoseNode x list) = show x ++ " " ++ show list

-- b. **Eq instance for RoseTree**

--    Write an `Eq` instance for `RoseTree a` (assuming `Eq a`).

instance Eq a => Eq (RoseTree a) where
  (RoseNode x xlist) == (RoseNode y ylist) = x == y && xlist == ylist

-- c. **Functor instance for RoseTree**

--    Write a `Functor` instance for `RoseTree`:
--    ```haskell
--    instance Functor RoseTree where
--        fmap :: (a -> b) -> RoseTree a -> RoseTree b
--    ```
--    The instance should apply the function to every value in the tree while preserving its shape. Verify the two functor laws:
--    - **Identity**: `fmap id t == t`
--    - **Composition**: `fmap (f . g) t == (fmap f . fmap g) t`

instance Functor RoseTree where
  fmap f (RoseNode x list) = RoseNode (f x) $ fmap (fmap f) list

-- d. **Foldable instance for RoseTree**

--    Write a `Foldable` instance for `RoseTree` by implementing `foldMap`:
--    ```haskell
--    instance Foldable RoseTree where
--        foldMap :: Monoid m => (a -> m) -> RoseTree a -> m
--    ```
--    The traversal order should be *pre-order*: process the root value first, then fold over the children left to right. Once the instance is defined, use it to implement:
--    - `roseToList :: RoseTree a -> [a]` — collects all values in pre-order
--    - `roseDepth  :: RoseTree a -> Int` — returns the depth of the tree (root has depth 1)

instance Foldable RoseTree where
  foldMap f (RoseNode x trees) = f x <> mconcat (fmap (foldMap f) trees)

roseToList :: RoseTree a -> [a]
roseToList = foldMap (: [])

-- alternative: achieve post-order having with pre-order foldMap
newtype InvList a = InvList {toList :: [a]} deriving Show
instance Semigroup (InvList a) where
  (InvList list1) <> (InvList list2) = InvList (list2 <> list2)
instance Monoid (InvList a) where
  mempty = InvList []
postRoseToList :: RoseTree a -> [a]
postRoseToList tree = toList $ foldMap (\t -> InvList [t]) tree

roseDepth :: RoseTree a -> Int
roseDepth tree = undefined


-- 1. **Implementing map and filter using folds**

--    Implement the functions `myMap :: (a -> b) -> [a] -> [b]` and `myFilter :: (a -> Bool) -> [a] -> [a]`
--    using both `foldr` and `foldl`. Compare their behaviour and performance in the context of lazy evaluation.

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldl (\s v -> s ++ [f v]) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter predicate = foldl (\seed x -> if predicate x then seed ++ [x] else seed) []


-- 2. **Fold with accumulation control**

--    Implement the function `foldlWithControl :: (b -> a -> Either b c) -> b -> [a] -> Either b c`, which
--    works like `foldl`, but allows aborting the computation at any point, returning the current accumulator
--    wrapped in `Left` or the final result in `Right`. Then use this function to implement:
--    - `findFirstThat :: (a -> Bool) -> [a] -> Maybe a` — finds the first element satisfying a predicate
--    - `takeWhileSum :: (Num a, Ord a) => a -> [a] -> [a]` — returns the longest prefix of a list whose sum does not exceed the given value
--    - `findSequence :: Eq a => [a] -> [a] -> Maybe Int` — finds the index of the first occurrence of a sublist in a list

foldlWithControl :: (b -> a -> Either b c) -> b -> [a] -> Either b c
foldlWithControl _ seed [] = Left seed
foldlWithControl f seed (x:xs) =
  case f seed x of
    Left seed' -> foldlWithControl f seed' xs
    Right control -> Right control

-- helper function
toMaybe :: Either a b -> Maybe b
toMaybe (Left _) = Nothing
toMaybe (Right x) = Just x

findFirstThat :: (a -> Bool) -> [a] -> Maybe a
findFirstThat predicate list = toMaybe $ foldlWithControl (\_ x -> if predicate x then Right x else Left ()) () list

-- extra assignment: do findSequence using foldlWithControl


-- 3. **Reversing folds**

--    Implement the function `unfoldl :: (b -> Maybe (b, a)) -> b -> [a]`, which is the inverse of `foldl` —
--    it generates a list from an initial state. Use it to implement:
--    - `countdown :: Int -> [Int]` — generates a countdown from n to 1
--    - `fib :: Int -> [Int]` — generates the first n Fibonacci numbers
--    - `iterate' :: (a -> a) -> a -> [a]` — your own implementation of the standard `iterate` function
--    - `decToBin :: Int -> [Int]` — converts a decimal number to its binary representation (a list of 0s and 1s)

unfoldl :: (b -> Maybe (b, a)) -> b -> [a]
unfoldl machine state = case machine state of
  Nothing -> []
  Just (state', observable) -> observable : unfoldl machine state'

countdown :: Int -> [Int]
countdown n = unfoldl machine state
  where
    machine 0 = Nothing
    machine m = Just (m-1, m)
    state = n

fib :: Int -> [Int]
fib n = unfoldl machine state
  where
    machine ((_, _), 0) = Nothing
    machine ((x, y), m) = Just (((y, x+y), m-1), x)
    state = ((0, 1), n)