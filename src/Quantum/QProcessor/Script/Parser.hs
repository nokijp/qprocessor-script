{-# LANGUAGE FlexibleContexts #-}

module Quantum.QProcessor.Script.Parser
  ( runScriptParser
  , parser
  ) where

import Control.Monad.State
import Data.Char hiding (Control)
import Data.Set (Set)
import qualified Data.Set as S
import Text.Parsec hiding (State)
import Quantum.QProcessor
import Quantum.QProcessor.Script.Syntax
import Quantum.QProcessor.Script.Internal.ParserCombinators

type SParser s = ParsecT String () (State s)
type ScriptParser = SParser (Set String)

runScriptParser :: ScriptParser Syntax -> String -> Either ParseError Syntax
runScriptParser p s = evalState (runParserT p () "" s) S.empty

parser :: ScriptParser Syntax
parser = foldr ($) NilOp <$> statements

statements :: ScriptParser [Syntax -> Syntax]
statements = line `sepBy` endOfLine

line :: ScriptParser (Syntax -> Syntax)
line = nonEolSpaces *> operation <* nonEolSpaces <* optional comment
  where
    comment = string "#" *> skipMany (satisfy (not . isEolChar)) <?> "comment"

operation :: ScriptParser (Syntax -> Syntax)
operation =
      transitionOperation
  <|> measureOperation
  <|> spyStateOperation
  <|> spyProbsOperation
  <|> newQVarOperation
  <|> emptyOperation

newQVarOperation :: ScriptParser (Syntax -> Syntax)
newQVarOperation = NewQVarOp
  <$> undeclaredQVarName <*> (nonEolSpaces *> string "=" *> nonEolSpaces *> try (string "newBit") *> nonEolSpaces1 *> bitValue)
  <?> "newVar statement"
    where
      bitValue = (Zero <$ char '0') <|> (One <$ char '1')

transitionOperation :: ScriptParser (Syntax -> Syntax)
transitionOperation = TransitionOp <$> transitionType <?> "transition statement"
  where
    transitionType =
          hadamardTransition
      <|> pauliXTransition
      <|> pauliYTransition
      <|> pauliZTransition
      <|> phaseTransition
      <|> cNotTransition
      <|> toffoliTransition
    hadamardTransition = Hadamard <$> (try (string "H") *> nonEolSpaces1 *> declaredQVarName)
    pauliXTransition = PauliX <$> (try (string "X") *> nonEolSpaces1 *> declaredQVarName)
    pauliYTransition = PauliY <$> (try (string "Y") *> nonEolSpaces1 *> declaredQVarName)
    pauliZTransition = PauliZ <$> (try (string "Z") *> nonEolSpaces1 *> declaredQVarName)
    phaseTransition = Phase <$> (try (string "R") *> nonEolSpaces1 *> float) <*> (nonEolSpaces1 *> declaredQVarName)
    cNotTransition = cnot <$> (try (string "CNOT") *> nonEolSpaces1 *> declaredQVarName) <*> (nonEolSpaces1 *> declaredQVarName)
    toffoliTransition = toffoli <$> (try (string "CCNOT") *> nonEolSpaces1 *> declaredQVarName) <*> (nonEolSpaces1 *> declaredQVarName) <*> (nonEolSpaces1 *> declaredQVarName)
    cnot c t = Control c (PauliX t)
    toffoli c1 c2 t = Control c1 (cnot c2 t)

measureOperation :: ScriptParser (Syntax -> Syntax)
measureOperation = MeasureOp <$> (try (string "measure") *> nonEolSpaces1 *> declaredQVarName) <?> "measure statement"

spyStateOperation :: Stream s m Char => ParsecT s u m (Syntax -> Syntax)
spyStateOperation = SpyStateOp <$ try (string "spyState") <?> "spyState statement"

spyProbsOperation :: Stream s m Char => ParsecT s u m (Syntax -> Syntax)
spyProbsOperation = SpyProbsOp <$ try (string "spyProbs") <?> "spyProbs statement"

emptyOperation :: Stream s m Char => ParsecT s u m (Syntax -> Syntax)
emptyOperation = id <$ string ""

qVarName :: Stream s m Char => ParsecT s u m String
qVarName = many1 alphaNum <?> "variable name"

declaredQVarName :: ScriptParser String
declaredQVarName = do
  vset <- get
  n <- qVarName
  when (S.notMember n vset) $ fail (n ++ " is not declared")
  return n

undeclaredQVarName :: ScriptParser String
undeclaredQVarName = do
  vset <- get
  n <- qVarName
  when (S.member n vset) $ fail (n ++ " is alreadly declared")
  put (S.insert n vset)
  return n

nonEolSpaces :: Stream s m Char => ParsecT s u m ()
nonEolSpaces = skipMany nonEolSpace

nonEolSpaces1 :: Stream s m Char => ParsecT s u m ()
nonEolSpaces1 = skipMany1 nonEolSpace

nonEolSpace :: Stream s m Char => ParsecT s u m ()
nonEolSpace = () <$ satisfy (\c -> isSpace c && not (isEolChar c)) <?> "white space"

isEolChar :: Char -> Bool
isEolChar c = c == '\r' || c == '\n'
