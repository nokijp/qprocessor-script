{-# LANGUAGE FlexibleContexts #-}

module Quantum.QProcessor.Script.Internal.ParserCombinators
  ( float
  ) where

import Data.Char
import Text.Parsec

float :: (Stream s m Char, Floating f, Read f) => ParsecT s u m f
float = do
  s <- (\a b c -> a ++ b ++ c) <$> sign <*> integerPart <*> option "" ((++) <$> string "." <*> fractionalPart)
  return $ read s
    where
      sign = option "" (string "-")
      integerPart = string "0" <|> ((:) <$> satisfy (\c -> isDigit c && c /= '0') <*> many digit)
      fractionalPart = many1 digit
