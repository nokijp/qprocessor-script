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

operation :: Parser (Syntax -> Syntax)
operation = newQVarOperation <|> emptyOperation

newQVarOperation :: Parser (Syntax -> Syntax)
newQVarOperation = NewQVarOp <$> qVarName <*> (nonEolSpaces1 *> spaces *> string "=" *> spaces *> string "newBit" *> many1 space *> bitValue)

emptyOperation :: Parser (Syntax -> Syntax)
emptyOperation = id <$ string ""

qVarName :: Parser String
qVarName = many1 alphaNum <?> "variable name"

bitValue :: Parser Bit
bitValue = (Zero <$ char '0') <|> (One <$ char '1')

comment :: Parser ()
comment = string "#" *> skipMany (satisfy (not . isEolChar)) <?> "comment"

nonEolSpaces :: Parser ()
nonEolSpaces = skipMany nonEolSpace

nonEolSpaces1 :: Parser ()
nonEolSpaces1 = skipMany1 nonEolSpace

nonEolSpace :: Parser ()
nonEolSpace = () <$ satisfy (\c -> isSpace c && not (isEolChar c)) <?> "white space"

isEolChar :: Char -> Bool
isEolChar c = c == '\r' || c == '\n'
