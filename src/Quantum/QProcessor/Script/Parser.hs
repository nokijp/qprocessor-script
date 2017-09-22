module Quantum.QProcessor.Script.Parser
  ( parser
  ) where

import Quantum.QProcessor
import Quantum.QProcessor.Script.Syntax

import Data.Char
import Text.Parsec
import Text.Parsec.String

parser :: Parser Syntax
parser = foldr ($) NilOp <$> statements

statements :: Parser [Syntax -> Syntax]
statements = line `sepBy` endOfLine

line :: Parser (Syntax -> Syntax)
line = nonEolSpaces *> operation <* nonEolSpaces <* optional comment
  where
    comment = string "#" *> skipMany (satisfy (not . isEolChar)) <?> "comment"

operation :: Parser (Syntax -> Syntax)
operation =
      transitionOperation
  <|> measureOperation
  <|> spyStateOperation
  <|> spyProbsOperation
  <|> newQVarOperation
  <|> emptyOperation

newQVarOperation :: Parser (Syntax -> Syntax)
newQVarOperation = NewQVarOp
  <$> qVarName <*> (nonEolSpaces *> string "=" *> nonEolSpaces *> string "newBit" *> nonEolSpaces1 *> bitValue)
  <?> "newVar statement"
    where
      bitValue = (Zero <$ char '0') <|> (One <$ char '1')

transitionOperation :: Parser (Syntax -> Syntax)
transitionOperation = TransitionOp <$> transitionType <?> "transition statement"
  where
    transitionType =
          hadamardTransition
      <|> pauliXTransition
      <|> pauliYTransition
      <|> pauliZTransition
      <|> cNotTransition
      <|> toffoliTransition
    hadamardTransition = Hadamard <$> (try (string "H") *> nonEolSpaces1 *> qVarName)
    pauliXTransition = PauliX <$> (try (string "X") *> nonEolSpaces1 *> qVarName)
    pauliYTransition = PauliY <$> (try (string "Y") *> nonEolSpaces1 *> qVarName)
    pauliZTransition = PauliZ <$> (try (string "Z") *> nonEolSpaces1 *> qVarName)
    cNotTransition = CNot <$> (try (string "CNOT") *> nonEolSpaces1 *> qVarName) <*> (nonEolSpaces1 *> qVarName)
    toffoliTransition = Toffoli <$> (try (string "CCNOT") *> nonEolSpaces1 *> qVarName) <*> (nonEolSpaces1 *> qVarName) <*> (nonEolSpaces1 *> qVarName)

measureOperation :: Parser (Syntax -> Syntax)
measureOperation = MeasureOp <$> (try (string "measure") *> nonEolSpaces1 *> qVarName) <?> "measure statement"

spyStateOperation :: Parser (Syntax -> Syntax)
spyStateOperation = SpyStateOp <$ try (string "spyState") <?> "spyState statement"

spyProbsOperation :: Parser (Syntax -> Syntax)
spyProbsOperation = SpyProbsOp <$ try (string "spyProbs") <?> "spyProbs statement"

emptyOperation :: Parser (Syntax -> Syntax)
emptyOperation = id <$ string ""

qVarName :: Parser String
qVarName = many1 alphaNum <?> "variable name"

nonEolSpaces :: Parser ()
nonEolSpaces = skipMany nonEolSpace

nonEolSpaces1 :: Parser ()
nonEolSpaces1 = skipMany1 nonEolSpace

nonEolSpace :: Parser ()
nonEolSpace = () <$ satisfy (\c -> isSpace c && not (isEolChar c)) <?> "white space"

isEolChar :: Char -> Bool
isEolChar c = c == '\r' || c == '\n'
