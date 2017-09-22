module Quantum.QProcessor.Script.Parser
  ( runVSParser
  , parser
  ) where

import Quantum.QProcessor
import Quantum.QProcessor.Script.Syntax

import Control.Monad.State
import Data.Char
import Data.Set (Set)
import qualified Data.Set as S
import Text.Parsec hiding (State)

type SParser s = ParsecT String () (State s)
type VSParser = SParser (Set String)

runVSParser :: VSParser Syntax -> String -> Either ParseError Syntax
runVSParser p s = evalState (runParserT p () "" s) S.empty

parser :: VSParser Syntax
parser = foldr ($) NilOp <$> statements

statements :: VSParser [Syntax -> Syntax]
statements = line `sepBy` endOfLine

line :: VSParser (Syntax -> Syntax)
line = nonEolSpaces *> operation <* nonEolSpaces <* optional comment
  where
    comment = string "#" *> skipMany (satisfy (not . isEolChar)) <?> "comment"

operation :: VSParser (Syntax -> Syntax)
operation =
      transitionOperation
  <|> measureOperation
  <|> spyStateOperation
  <|> spyProbsOperation
  <|> newQVarOperation
  <|> emptyOperation

newQVarOperation :: VSParser (Syntax -> Syntax)
newQVarOperation = NewQVarOp
  <$> undeclaredQVarName <*> (nonEolSpaces *> string "=" *> nonEolSpaces *> string "newBit" *> nonEolSpaces1 *> bitValue)
  <?> "newVar statement"
    where
      bitValue = (Zero <$ char '0') <|> (One <$ char '1')

transitionOperation :: VSParser (Syntax -> Syntax)
transitionOperation = TransitionOp <$> transitionType <?> "transition statement"
  where
    transitionType =
          hadamardTransition
      <|> pauliXTransition
      <|> pauliYTransition
      <|> pauliZTransition
      <|> cNotTransition
      <|> toffoliTransition
    hadamardTransition = Hadamard <$> (try (string "H") *> nonEolSpaces1 *> declaredQVarName)
    pauliXTransition = PauliX <$> (try (string "X") *> nonEolSpaces1 *> declaredQVarName)
    pauliYTransition = PauliY <$> (try (string "Y") *> nonEolSpaces1 *> declaredQVarName)
    pauliZTransition = PauliZ <$> (try (string "Z") *> nonEolSpaces1 *> declaredQVarName)
    cNotTransition = CNot <$> (try (string "CNOT") *> nonEolSpaces1 *> declaredQVarName) <*> (nonEolSpaces1 *> declaredQVarName)
    toffoliTransition = Toffoli <$> (try (string "CCNOT") *> nonEolSpaces1 *> declaredQVarName) <*> (nonEolSpaces1 *> declaredQVarName) <*> (nonEolSpaces1 *> declaredQVarName)

measureOperation :: VSParser (Syntax -> Syntax)
measureOperation = MeasureOp <$> (try (string "measure") *> nonEolSpaces1 *> declaredQVarName) <?> "measure statement"

spyStateOperation :: VSParser (Syntax -> Syntax)
spyStateOperation = SpyStateOp <$ try (string "spyState") <?> "spyState statement"

spyProbsOperation :: VSParser (Syntax -> Syntax)
spyProbsOperation = SpyProbsOp <$ try (string "spyProbs") <?> "spyProbs statement"

emptyOperation :: VSParser (Syntax -> Syntax)
emptyOperation = id <$ string ""

qVarName :: VSParser String
qVarName = many1 alphaNum <?> "variable name"

declaredQVarName :: VSParser String
declaredQVarName = do
  vset <- get
  n <- qVarName
  when (S.notMember n vset) $ fail (n ++ " is not declared")
  return n

undeclaredQVarName :: VSParser String
undeclaredQVarName = do
  vset <- get
  n <- qVarName
  when (S.member n vset) $ fail (n ++ " is alreadly declared")
  put (S.insert n vset)
  return n

nonEolSpaces :: VSParser ()
nonEolSpaces = skipMany nonEolSpace

nonEolSpaces1 :: VSParser ()
nonEolSpaces1 = skipMany1 nonEolSpace

nonEolSpace :: VSParser ()
nonEolSpace = () <$ satisfy (\c -> isSpace c && not (isEolChar c)) <?> "white space"

isEolChar :: Char -> Bool
isEolChar c = c == '\r' || c == '\n'
