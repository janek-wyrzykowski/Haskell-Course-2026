module ATPL (parseProgram, pretty) where

import Control.Monad.State
import Data.Char
import Data.Map ( fromList, Map )

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
data Position = Position {posLine :: Int, posCol :: Int} deriving (Show, Eq)

instance Ord Position where
  Position al ac <= Position bl bc = 
    if al == bl
    then ac <= bc 
    else al <= bl

data ParseError = ParseError {errorPos :: Position, errorMsg :: String} deriving (Show, Eq)

data ParseState = ParseState {input :: String, position :: Position} deriving (Show, Eq)

type Parser a = StateT ParseState (Either ParseError) a

runParser :: Parser a -> String -> Either ParseError (a, ParseState)
runParser p s = runStateT p (ParseState s (Position 1 1))

failParse :: String -> Parser a
failParse msg = StateT $ \s -> Left $ ParseError (position s) msg

advance :: Char -> Position -> Position
advance '\n' (Position l _) = Position (l + 1) 1
advance _ (Position l c) = Position l (c + 1)

(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = StateT $ \s ->
  case runStateT p1 s of
    Right r -> Right r
    Left err1 -> case runStateT p2 s of
      Right r -> Right r
      Left err2 -> do
        let pos1 = errorPos err1
            pos2 = errorPos err2
        Left $ if pos1 >= pos2 then err1 else err2

infixr 5 <|>

label :: String -> Parser a -> Parser a
label message parser = StateT $ \s ->
  case runStateT parser s of
    Left (ParseError p _) -> Left (ParseError p message)
    Right r -> Right r

-- ~~~~~ Building blocks ~~~~~
zero :: Parser a
zero = failParse "Parse error"

item :: Parser Char
item = do
  s <- get
  case input s of
    [] -> failParse "Unexpected end of input"
    c : cs -> do
      put $ ParseState cs (advance c (position s))
      pure c

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

-- ~~~~~ Formula parsing ~~~~~
lowerWord :: Parser String
lowerWord = do
  word <- label "Expected a snake_case word" $ many1 (sat isLower <|> char '_')
  spaces
  pure word

upperWord :: Parser String
upperWord = do
  word <- label "Expected a CONSTANT_CASE word" $ many1 (sat isUpper <|> char '_')
  spaces
  pure word

parseFormula, parseTerm :: Parser Formula
parseFormula = parseTerm `chainl1` binop
parseTerm = parseNot <|> parseVar <|> parseConst <|> parseParen
  where
    parseVar = do
      var <- token upperWord
      pure (Var var)
    parseConst =
      label "Expected one of '0', '1'" (symbol "0" >> pure (Const False))
        <|> (symbol "1" >> pure (Const True))
    parseNot = do
      _ <- label "Expected '~'" $ symbol "~"
      Not <$> parseFormula
    parseParen = do
      _ <- label "Expected '('" $ symbol "("
      f <- parseFormula
      _ <- label "Unclosed parentheses" $ symbol ")"
      pure f

binop :: Parser (Formula -> Formula -> Formula)
binop =
  label "Unknown binary operator" (symbol "/\\" >> pure (BinOp And))
    <|> (symbol "\\/" >> pure (BinOp Or))
    <|> (symbol "->" >> pure (BinOp Implies))
    <|> (symbol "<=>" >> pure (BinOp Iff))

-- ~~~~~ Program parsing ~~~~~
parseReasoning :: String -> Parser Reasoning
parseReasoning stepType = do
  case stepType of
    "intro" -> pure Intro
    "exact" -> pure Exact
    "have" -> do
      label "Expected 'by' keyword after the formula in a 'have' proof step" $ symbol "by"
      reasoningName <- lowerWord
      reasoningArgs <- many lowerWord
      pure $ Reasoning reasoningName reasoningArgs

parseProofStep :: Parser (String, ProofStep)
parseProofStep = do
  spaces
  stepType <- label "Expected one of: 'intro', 'have', 'exact'" $ symbol "intro" <|> symbol "have" <|> symbol "exact"
  stepName <- lowerWord
  label "Expected ':' after the proof step name" $ symbol ":"
  formula <- parseFormula
  reasoning <- parseReasoning stepType
  label "Expected ';' at the end of the proof step" $ symbol ";"
  pure (stepName, ProofStep (formula, reasoning))

parseTheorem :: Parser (String, Statement)
parseTheorem = do
  label "Expected 'theorem' keyword" $ symbol "theorem"
  theoremName <- lowerWord
  label "Expected ':' after the theorem name" $ symbol ":"
  formula <- parseFormula
  label "Expected 'proof' keyword" $ symbol "proof"
  proofSteps <- many1 parseProofStep
  label "Expected 'qed' keyword" $ symbol "qed"
  pure (theoremName, Theorem formula (Proof (fromList proofSteps)))

parseAxiom :: Parser (String, Statement)
parseAxiom = do
  label "Expected 'axiom' keyword" $ symbol "axiom"
  axiomName <- lowerWord
  label "Expected ':' after the axiom name" $ symbol ":"
  formula <- parseFormula
  pure (axiomName, Axiom formula)

parseStatement :: Parser (String, Statement)
parseStatement = parseTheorem <|> parseAxiom

parseProg :: Parser Program
parseProg = do
  statements <- many1 parseStatement
  pure $ Program (fromList statements)

-- TODO: write error handling
parseProgram :: String -> Either ParseError Program
parseProgram s =
  case runParser (spaces >> parseProg) s of
    Right (prog, ParseState {input = ""}) -> Right prog
    Right (_, st) -> Left $ ParseError (position st) "Unexpected trailing input"
    Left err -> Left err

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
-- ~~~~~ Pretty printer ~~~~~
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~

-- ~~~~~ Top-level function ~~~~~
pretty :: Program -> String
pretty p = undefined

-- ~~~~~~~~~~~~~~~~~~~~~
-- ~~~~~ Evaluator ~~~~~
-- ~~~~~~~~~~~~~~~~~~~~~
