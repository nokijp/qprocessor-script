{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}

module Quantum.QProcessor.Script.ParserSpec
  ( main
  , spec
  ) where

import Test.Hspec

import Quantum.QProcessor
import Quantum.QProcessor.Script.Parser
import Quantum.QProcessor.Script.Syntax

import Control.Monad
import Text.InterpolatedString.Perl6
import Text.Parsec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parser" $ do
    forM_
      [ ("emptyInput", emptyInput, emptySyntax)
      , ("smallInput", smallInput, smallSyntax)
      , ("delimiterInput1", delimiterInput1, delimiterSyntax1)
      , ("delimiterInput2", delimiterInput2, delimiterSyntax2)
      , ("delimiterInput3", delimiterInput3, delimiterSyntax3)
      , ("delimiterInput4", delimiterInput4, delimiterSyntax4)
      ] $ \(name, input, syntax) ->
        it ("should accept " ++ name) $ parse (parser <* eof) "" input `shouldBe` Right syntax

emptyInput :: String
emptyInput = ""

emptySyntax :: Syntax
emptySyntax = NilOp

smallInput :: String
smallInput = [q|q = newBit 0|]

smallSyntax :: Syntax
smallSyntax = NewQVarOp "q" Zero NilOp

delimiterInput1 :: String
delimiterInput1 = [q|  # comment


q0 = newBit 0  # comment

q1 = newBit 1# comment
q2 = newBit 0#

#comment|]

delimiterSyntax1 :: Syntax
delimiterSyntax1 = NewQVarOp "q0" Zero (NewQVarOp "q1" One (NewQVarOp "q2" Zero NilOp))

delimiterInput2 :: String
delimiterInput2 = [q|# comment
q0 = newBit 0  # comment
|]

delimiterSyntax2 :: Syntax
delimiterSyntax2 = NewQVarOp "q0" Zero NilOp

delimiterInput3 :: String
delimiterInput3 = [q|
q = newBit 0

# comment
|]

delimiterSyntax3 :: Syntax
delimiterSyntax3 = NewQVarOp "q" Zero NilOp

delimiterInput4 :: String
delimiterInput4 = "# comment"

delimiterSyntax4 :: Syntax
delimiterSyntax4 = NilOp
