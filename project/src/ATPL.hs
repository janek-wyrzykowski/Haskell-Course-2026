module ATPL (parseProgram, pretty) where

import Control.Monad.State
import Data.Char (isLower, isSpace, isUpper)
import Data.Map

-- ~~~~~~~~~~~~~~~
-- ~~~~~ AST ~~~~~
-- ~~~~~~~~~~~~~~~

newtype Program = Program (Map String Statement) deriving (Show, Eq)

data Statement
  = Axiom Formula
  | Theorem Formula Proof
  deriving (Show, Eq)

newtype Proof = Proof {proofSteps :: Map String ProofStep} deriving (Show, Eq)

newtype ProofStep = ProofStep (Formula, Reasoning) deriving (Show, Eq)

data Reasoning = Reasoning String [String] | Intro | Exact deriving (Show, Eq)

data Formula
  = Var String
  | Const Bool
  | Not Formula
  | BinOp LogOp Formula Formula
  deriving (Show, Eq)

data LogOp = And | Or | Implies | Iff deriving (Show, Eq)

-- ~~~~~~~~~~~~~~~~~~
-- ~~~~~ Parser ~~~~~
-- ~~~~~~~~~~~~~~~~~~

-- ~~~~~ Type definition ~~~~~
type Parser a = StateT String [] a

runParser :: Parser a -> String -> [(a, String)]
runParser = runStateT

-- ~~~~~ Basics ~~~~~
zero :: Parser a
zero = StateT (const [])

item :: Parser Char
item = do
  s <- get
  case s of
    c : cs -> put cs >> pure c
    [] -> zero

(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = StateT $ \s ->
  case runStateT p1 s of
    [] -> runStateT p2 s
    parses -> parses

infixr 5 <|>

-- ~~~~~ Building blocks ~~~~~
sat :: (Char -> Bool) -> Parser Char
sat predicate = do
  c <- item
  if predicate c then pure c else zero

char :: Char -> Parser Char
char c = sat (== c)

-- digit :: Parser Char
-- digit = sat isDigit

spaceP :: Parser Char
spaceP = sat isSpace

many :: Parser a -> Parser [a]
many p = many1 p <|> pure []

many1 :: Parser a -> Parser [a]
many1 p = do
  x <- p
  xs <- many p
  pure (x : xs)

string :: String -> Parser String
string [] = pure []
string (c : cs) = do
  _ <- char c
  _ <- string cs
  pure (c : cs)

spaces :: Parser ()
spaces = do
  _ <- many spaceP
  pure ()

token :: Parser a -> Parser a
token p = do
  v <- p
  spaces
  pure v

symbol :: String -> Parser String
symbol cs = token (string cs)

-- nat :: Parser Natural
-- nat = do
--   ds <- many1 digit
--   pure (read ds)

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= rest
  where
    rest x =
      ( do
          f <- op
          y <- p
          rest (f x y)
      )
        <|> pure x

lowerWord :: Parser String
lowerWord = do
  word <- many1 (sat isLower <|> char '_')
  spaces
  pure word

upperWord :: Parser String
upperWord = do
  word <- many1 (sat isUpper <|> char '_')
  spaces
  pure word

-- ~~~~~ Formula parsing ~~~~~
parseFormula, parseTerm :: Parser Formula
parseFormula = parseTerm `chainl1` binop
parseTerm = parseNot <|> parseVar <|> parseConst <|> parseParen
  where
    parseVar = do
      var <- token upperWord
      pure (Var var)
    parseConst =
      (symbol "0" >> pure (Const False))
        <|> (symbol "1" >> pure (Const True))
    parseNot = do
      _ <- symbol "~"
      Not <$> parseFormula
    parseParen = do
      _ <- symbol "("
      f <- parseFormula
      _ <- symbol ")"
      pure f

binop :: Parser (Formula -> Formula -> Formula)
binop =
  (symbol "/\\" >> pure (BinOp And))
    <|> (symbol "\\/" >> pure (BinOp Or))
    <|> (symbol "->" >> pure (BinOp Implies))
    <|> (symbol "<=>" >> pure (BinOp Iff))

-- ~~~~~ Program parsing ~~~~~
parseReasoning :: String -> Parser Reasoning
parseReasoning stepType = do
  reasoningName <- lowerWord
  reasoningArgs <- many lowerWord
  case stepType of
    "intro" -> pure Intro
    "have" -> pure $ Reasoning reasoningName reasoningArgs
    "exact" -> pure Exact

parseProofStep :: Parser (String, ProofStep)
parseProofStep = do
  spaces
  stepType <- symbol "intro" <|> symbol "have" <|> symbol "exact"
  stepName <- lowerWord
  symbol ":"
  formula <- parseFormula
  symbol "by"
  reasoning <- parseReasoning stepType
  symbol ";"
  pure (stepName, ProofStep (formula, reasoning))

parseTheorem :: Parser (String, Statement)
parseTheorem = do
  symbol "theorem"
  theoremName <- lowerWord
  symbol ":"
  formula <- parseFormula
  symbol "proof"
  proofSteps <- many1 parseProofStep
  symbol "qed"
  pure (theoremName, Theorem formula (Proof (fromList proofSteps)))

parseAxiom :: Parser (String, Statement)
parseAxiom = do
  symbol "axiom"
  axiomName <- lowerWord
  symbol ":"
  formula <- parseFormula
  pure (axiomName, Axiom formula)

parseStatement :: Parser (String, Statement)
parseStatement = parseTheorem <|> parseAxiom

parseProg :: Parser Program
parseProg = do
  statements <- many1 parseStatement
  pure $ Program (fromList statements)

-- TODO: write error handling
parseProgram :: String -> Maybe Program
parseProgram s =
  case runParser (spaces >> parseProg) s of
    ((e, "") : _) -> Just e
    _ -> Nothing

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
-- ~~~~~ Pretty printer ~~~~~
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~

-- ~~~~~ Top-level function ~~~~~
pretty :: Program -> String
pretty p = undefined

-- ~~~~~~~~~~~~~~~~~~~~~
-- ~~~~~ Evaluator ~~~~~
-- ~~~~~~~~~~~~~~~~~~~~~
