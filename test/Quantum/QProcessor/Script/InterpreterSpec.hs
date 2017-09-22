module Quantum.QProcessor.Script.InterpreterSpec
  ( main
  , spec
  ) where

import Test.Hspec

import Quantum.QProcessor
import Quantum.QProcessor.Script.Interpreter
import Quantum.QProcessor.Script.Syntax

import Control.Monad

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "interpretList" $ do
    forM_
      [ ("simpleSyntax", simpleSyntax, simpleOutput)
      ] $ \(name, syntax, output) -> do
        actualOutput <- runIO $ interpretList syntax
        it ("can interpret " ++ name) $ actualOutput `shouldBe` Just output
    forM_
      [ ("duplicateVariableSyntax", duplicateVariableSyntax)
      , ("undeclaredVariableSyntax", undeclaredVariableSyntax)
      ] $ \(name, syntax) -> do
        actualOutput <- runIO $ interpretList syntax
        it ("should return Nothing when given " ++ name) $ actualOutput `shouldBe` Nothing

simpleSyntax :: Syntax
simpleSyntax =
    NewQVarOp "q0" One
  $ MeasureOp "q0"
  $ NewQVarOp "q1" One
  $ TransitionOp (PauliY "q1")
  $ SpyStateOp
  $ TransitionOp (Hadamard "q1")
  $ NewQVarOp "q2" Zero
  $ SpyProbsOp
  $ NilOp

simpleOutput :: [String]
simpleOutput =
  [ "measure: |1>"
  , "state: |0> 0.0000 + 0.0000i, |1> 0.0000 - 1.0000i, |2> 0.0000 + 0.0000i, |3> 0.0000 + 0.0000i"
  , "probs: |0> 0.0000, |1> 0.5000, |2> 0.0000, |3> 0.5000, |4> 0.0000, |5> 0.0000, |6> 0.0000, |7> 0.0000"
  ]

duplicateVariableSyntax :: Syntax
duplicateVariableSyntax = NewQVarOp "q" One $ NewQVarOp "q" One NilOp

undeclaredVariableSyntax :: Syntax
undeclaredVariableSyntax = MeasureOp "q1" NilOp
