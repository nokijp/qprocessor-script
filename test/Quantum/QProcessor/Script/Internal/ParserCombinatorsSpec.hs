{-# LANGUAGE ScopedTypeVariables #-}

module Quantum.QProcessor.Script.Internal.ParserCombinatorsSpec
  ( main
  , spec
  ) where

import Test.Hspec
import Test.Hspec.Expectations.Contrib

import Quantum.QProcessor.Script.Internal.ParserCombinators

import Control.Monad
import Text.Parsec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "float" $ do
    forM_
      [ ("1", 1)
      , ("-1", -1)
      , ("1.00000", 1)
      , ("0.5", 0.5)
      , ("-0.5", -0.5)
      , ("100.0625", 100.0625)
      , ("-100.0625", -100.0625)
      ] $ \(input, expected :: Double) ->
        it ("should accept " ++ show input) $ parse (float <* eof) "" input `shouldBe` Right expected
    forM_
      [ ""
      , ".1"
      , "1."
      , "01"
      , "01.5"
      , "-01"
      ] $ \input ->
        it ("should return an error when given " ++ show input) $ isLeft $ (parse (float <* eof) "" input :: Either ParseError Double)
