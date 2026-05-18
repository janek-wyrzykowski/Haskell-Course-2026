module Tutorials05 where

import Control.Monad.State
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Char

-- 1. **State Monad for tracking state**

--    Define a function `runningSum :: [Int] -> [Int]` that, given a list of integers, returns a list of partial sums.
--    For example, for the list `[1, 2, 3, 4]` the result should be `[1, 3, 6, 10]`. Implement this function
--    using the State monad, making use of `get`, `put`, and `runState` or `evalState`.

-- 2. **A pseudo-random number generation using State monad**

--    Implement a simple pseudo-random number generator using the State monad. Define a function
--    `randomInt :: Int -> Int -> State Int Int` that generates an integer in the given range `[a, b]`,
--    using a [linear congruential generator](https://en.wikipedia.org/wiki/Linear_congruential_generator).
--    Then write a function `randomList :: Int -> Int -> Int -> State Int [Int]` that generates a list
--    of `n` random numbers from the range `[a, b]`. Use `evalState` to run the computation with a given seed.

-- 3. **Binary tree and labelling with State**

--    Define a binary tree type `data Tree a = Empty | Node a (Tree a) (Tree a)`. Then implement a function
--    `labelTree :: Tree a -> State Int (Tree (a, Int))` that labels each node of the tree with a unique number,
--    using the State monad to track the counter. The numbering should be in preorder.
--    Also write a function `countNodes :: Tree a -> State (Sum Int) (Tree a)` that counts the nodes in the tree,
--    using the State monad for accumulation.

-- 4. **Interactive calculation using IO**

--    Write a program `calculator :: IO ()` that reads two numbers and an operation (addition, subtraction,
--    multiplication, division) from the user and prints the result. The program should handle errors
--    (e.g. division by zero) and ask the user whether they want to continue. Use `getLine`, `readLn`,
--    and `putStrLn` to interact with the user.

calculator :: IO ()
calculator = do
  print "Input two integers"
  x <- readLn :: IO Int
  y <- readLn :: IO Int
  print "Input operation (sum, dif, mul, div)"
  operation <- getLine
  print $ translate operation x y
  where
    translate :: String -> (Int -> Int -> Int)
    translate "sum" = (+)
    translate "dif" = (-)
    translate "mul" = (*)
    translate "div" = div

-- 5. **A safer calculator with MaybeT**
--
--    The `calculator :: IO ()` from task 4 uses `readLn :: IO Int`, which throws a runtime exception
--    whenever the user types something that is not a valid integer (e.g. `"abc"` or an empty line).
--    Rewrite the calculator using the `MaybeT` transformer from `Control.Monad.Trans.Maybe`
--    so that bad input is reported as `Nothing` instead of crashing the program.
--
--    * Define a helper `readInt :: MaybeT IO Int` that reads a line from standard input and
--      produces `Nothing` when the line is not a valid integer (use `readMaybe` from `Text.Read`,
--      or check the input by hand with `Data.Char.isDigit`).
--    * Define a helper `readOp :: MaybeT IO (Int -> Int -> Int)` that reads an operation name
--      (e.g. `"sum"`, `"difference"`, `"product"`) and returns the corresponding function, or
--      `Nothing` if the name is not recognised.
--    * Implement `goodCalculator :: MaybeT IO ()` that reads two integers and an operation
--      using the helpers above and prints the result.

type ErrorIO a = MaybeT IO a

readName :: ErrorIO String
readName = do
  line <- lift getLine
  if predicate line then pure line else hoistMaybe Nothing
  where
    predicate :: String -> Bool
    predicate (x : _) = isUpper x
    predicate _ = False

readInt :: ErrorIO Int
readInt = do
  line <- lift getLine
  pure 0
  where
    allDigits :: String -> Bool
    allDigits line = undefined

-- 6. **StateT — state on top of another monad**

--    Recall `State s a ≅ s -> (a, s)`. Wrapping the result in an
--    arbitrary monad `m` gives the state monad transformer:

--    ```haskell
--    newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }
--    ```
--    A value of type `StateT s m a` is a stateful step whose result lives in `m`.

newtype StateT' s m a = StateT' {runStateT' :: s -> m (a, s)}

--    **(a) The `Monad` instance.** Complete the following definition:

--    ```haskell
--    instance Monad m => Monad (StateT s m) where
--      return a          = StateT $ \s -> ???
--      (StateT g) >>= f  = StateT $ \s -> ???
--    ```

instance (Functor m) => Functor (StateT' s m) where
  fmap f (StateT' g) = StateT' $ fmap fxid . g
    where
      fxid (x, s) = (f x, s)

instance (Monad m) => Applicative (StateT' s m) where
  pure x = StateT' $ \state -> pure (x, state)
  liftA2 f (StateT' g) (StateT' h) = StateT' $ \state -> do
    (x, state') <- g state
    (y, state'') <- h state'
    return (f x y, state'')

instance (Monad m) => Monad (StateT' s m) where
  (StateT' g) >>= f = StateT' $ \state -> do
    (x, state') <- g state
    (runStateT' . f) x state'

--    **(b) The `MonadTrans` instance.** Complete the lifting operation:

--    ```haskell
--    instance MonadTrans (StateT s) where
--      lift ma = StateT $ \s -> ???   -- use fmap to pair the result of ma with s
--    ```

instance MonadTrans (StateT' s) where
  lift ma = StateT' $ \state -> do
    x <- ma
    return (x, state)

-- 7. **Combining StateT and IO**

--    Implement a simple ATM simulator using the StateT transformer. Define a type `BankState` containing
--    the account balance. Write the following functions:
--    * `withdraw :: Int -> StateT BankState IO Bool` — attempts to withdraw a given amount
--    * `deposit :: Int -> StateT BankState IO ()` — deposits a given amount
--    * `checkBalance :: StateT BankState IO Int` — checks the current balance
--    * `atmSession :: StateT BankState IO ()` — runs an interactive session with the user

--    Each operation should print appropriate messages on the screen and update the account state.

newtype BankState = BankState {balance :: Int} deriving (Show, Eq, Ord)

type ATM = StateT BankState IO

withdraw :: Int -> ATM Bool
withdraw value = do
  bankState <- get
  if balance bankState >= value
    then do
      let newBankState = BankState $ balance bankState - value
      put newBankState
      return True
    else do
      lift $ print "Withdrawal not possible"
      return False

deposit :: Int -> ATM ()
deposit = undefined

checkBalance :: ATM Int
checkBalance = do
  bankState <- get
  lift $ putStrLn $ "The current balance is " ++ show (balance bankState)
  return $ balance bankState

atmSession :: ATM ()
atmSession = do
  lift $ putStrLn "Press d for Deposit, c for Check Balance"
  char <- lift getChar
  case char of
    'd' -> do
      value <- lift (readLn :: IO Int)
      withdraw value
      atmSession
    'c' -> do
      checkBalance
      atmSession
    _ -> do
      lift $ putStrLn "Unsupported functionality"

-- 8. **Implementing a stack of transformers**

--    Define a type `AppM a = ReaderT Config (StateT AppState (ExceptT AppError IO)) a`, where:
--    * `Config` contains configuration parameters (e.g. `maxAttempts :: Int`)
--    * `AppState` contains the application state (e.g. `counter :: Int`, `lastOperation :: String`)
--    * `AppError` is a type representing possible errors (e.g. `NetworkError String`, `ValidationError String`)

--    Then implement the following helper functions:
--    * `getConfig :: AppM Config` — retrieves the configuration
--    * `getState :: AppM AppState` — retrieves the state
--    * `modifyState :: (AppState -> AppState) -> AppM ()` — modifies the state
--    * `throwAppError :: AppError -> AppM a` — raises an error
--    * `runApp :: Config -> AppState -> AppM a -> IO (Either AppError (a, AppState))` — runs the computation

--    Finally, implement an example business function `processTransaction :: Transaction -> AppM Result`
--    that uses the helper functions above.