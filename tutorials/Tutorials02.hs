module Tutorials02 where


-- 5. Tail Recursion and Expression Evaluation Using an Explicit Stack

data Expr = Lit Int | Add Expr Expr | Mul Expr Expr deriving (Show, Eq)

-- a) naive recursive - not tail recursive, but works well
-- a stack is used internally anyway
naiveEval :: Expr -> Int
naiveEval (Lit n) = n
naiveEval (Add e1 e2) = naiveEval e1 + naiveEval e2
naiveEval (Mul e1 e2) = naiveEval e1 * naiveEval e2

-- b) tail-recursive with a stack
data StackEntry = Apply (Int -> Int -> Int) Int | ComputeLeft (Int -> Int -> Int) Expr

eval :: Expr -> Int
eval expr = go [] expr -- stack goes first
  where
    go [] (Lit n) = n
    go (Apply f m : stack) (Lit n) = go stack (Lit $ f m n)
    go (ComputeLeft f e : stack) (Lit n) = go (Apply f n : stack) e
    go stack (Add e1 e2) = go (ComputeLeft (+) e1 : stack) e2
    go stack (Mul e1 e2) = go (ComputeLeft (*) e1 : stack) e2

-- Example:
-- go [] Mul (Add 1 2) (Add 3 4)
-- go [CL (*) (Add 1 2)] (Add 3 4)
-- go [CL (+) 3, CL (*) (Add 1 2)] 4
-- go [A (+) 4, CL (*) (Add 1 2)] 3
-- go [CL (*) (Add 1 2)] 7
-- go [A (*) 7] (Add 1 2)
-- go [CL (+) 1, A (*) 7] 2
-- go [A (+) 2, A (*) 7] 1
-- go [A (*) 7] 3
-- 21