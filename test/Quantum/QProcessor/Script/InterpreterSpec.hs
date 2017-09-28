module Quantum.QProcessor.Script.InterpreterSpec
  ( main
  , spec
  ) where

import Test.Hspec

import Data.List
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
      , ("allOpsSyntax", allOpsSyntax, allOpsOutput)
      , ("spyProbsPartialSyntax", spyProbsPartialSyntax, spyProbsPartialOutput)
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
  $ MeasureOp ["q0"]
  $ NewQVarOp "q1" Zero
  $ MeasureOp ["q0", "q1"]
  $ TransitionOp (PauliX "q1")
  $ TransitionOp (PauliY "q1")
  $ SpyStateOp
  $ TransitionOp (Hadamard "q1")
  $ NewQVarOp "q2" Zero
  $ TransitionOp (Control "q1" (PauliX "q2"))
  $ SpyProbsOp
  $ DiagramOp
  $ NilOp

simpleOutput :: [String]
simpleOutput =
  [ "measure: |1>"
  , "measure: |1>|0>"
  , "state: (0.0000 + 0.0000i)|0> + (0.0000 - 1.0000i)|1> + (0.0000 + 0.0000i)|2> + (0.0000 + 0.0000i)|3>"
  , "probs: |0> 0.0000, |1> 0.5000, |2> 0.0000, |3> 0.0000, |4> 0.0000, |5> 0.0000, |6> 0.0000, |7> 0.5000"
  , intercalate "\n"
      [ "|1>--@--@--------------"
      , "                       "
      , "|0>-----@--X--Y--H--*--"
      , "                    |  "
      , "|0>-----------------X--"
      ]
  ]

allOpsSyntax :: Syntax
allOpsSyntax =
    NewQVarOp "q0" Zero
  $ NewQVarOp "q1" Zero
  $ NewQVarOp "q2" One
  $ MeasureOp ["q0", "q1", "q2"]
  $ SpyStateOp
  $ SpyProbsOp
  $ SpyProbsPartialOp ["q0"]
  $ TransitionOp (Hadamard "q0")
  $ TransitionOp (PauliX "q0")
  $ TransitionOp (PauliY "q0")
  $ TransitionOp (PauliZ "q0")
  $ TransitionOp (Phase 0.5 "q0")
  $ TransitionOp (Not "q0")
  $ TransitionOp (Control "q0" (Control "q1" (Not "q2")))
  $ DiagramOp
  $ NilOp

allOpsOutput :: [String]
allOpsOutput =
  [ "measure: |0>|0>|1>"
  , "state: (0.0000 + 0.0000i)|0> + (0.0000 + 0.0000i)|1> + (0.0000 + 0.0000i)|2> + (0.0000 + 0.0000i)|3> + (1.0000 + 0.0000i)|4> + (0.0000 + 0.0000i)|5> + (0.0000 + 0.0000i)|6> + (0.0000 + 0.0000i)|7>"
  , "probs: |0> 0.0000, |1> 0.0000, |2> 0.0000, |3> 0.0000, |4> 1.0000, |5> 0.0000, |6> 0.0000, |7> 0.0000"
  , "probs: |0> 1.0000, |1> 0.0000"
  , intercalate "\n"
      [ "|0>--@--H--X--Y--Z--R(0.5000)--⊕--*--"
      , "                                  |  "
      , "|0>--@----------------------------*--"
      , "                                  |  "
      , "|1>--@----------------------------⊕--"
      ]
  ]

spyProbsPartialSyntax :: Syntax
spyProbsPartialSyntax =
    NewQVarOp "q0" Zero
  $ NewQVarOp "q1" One
  $ NewQVarOp "q2" Zero
  $ SpyProbsPartialOp ["q0"]
  $ SpyProbsPartialOp ["q0", "q1"]
  $ SpyProbsPartialOp ["q1", "q0"]
  $ SpyProbsPartialOp ["q0", "q0", "q0", "q0"]
  $ TransitionOp (Hadamard "q0")
  $ TransitionOp (Hadamard "q1")
  $ TransitionOp (Control "q0" (Not "q2"))
  $ SpyProbsPartialOp ["q0"]
  $ SpyProbsPartialOp ["q0", "q1"]
  $ SpyProbsPartialOp ["q0", "q2"]
  $ SpyProbsPartialOp ["q0", "q1", "q2"]
  $ SpyProbsPartialOp ["q0", "q0", "q0", "q0"]
  $ NilOp

spyProbsPartialOutput :: [String]
spyProbsPartialOutput =
  [ "probs: |0> 1.0000, |1> 0.0000"
  , "probs: |0> 0.0000, |1> 0.0000, |2> 1.0000, |3> 0.0000"
  , "probs: |0> 0.0000, |1> 1.0000, |2> 0.0000, |3> 0.0000"
  , "probs: |0> 1.0000, |1> 0.0000, |2> 0.0000, |3> 0.0000, |4> 0.0000, |5> 0.0000, |6> 0.0000, |7> 0.0000, |8> 0.0000, |9> 0.0000, |10> 0.0000, |11> 0.0000, |12> 0.0000, |13> 0.0000, |14> 0.0000, |15> 0.0000"
  , "probs: |0> 0.5000, |1> 0.5000"
  , "probs: |0> 0.2500, |1> 0.2500, |2> 0.2500, |3> 0.2500"
  , "probs: |0> 0.5000, |1> 0.0000, |2> 0.0000, |3> 0.5000"
  , "probs: |0> 0.2500, |1> 0.0000, |2> 0.2500, |3> 0.0000, |4> 0.0000, |5> 0.2500, |6> 0.0000, |7> 0.2500"
  , "probs: |0> 0.5000, |1> 0.0000, |2> 0.0000, |3> 0.0000, |4> 0.0000, |5> 0.0000, |6> 0.0000, |7> 0.0000, |8> 0.0000, |9> 0.0000, |10> 0.0000, |11> 0.0000, |12> 0.0000, |13> 0.0000, |14> 0.0000, |15> 0.5000"
  ]

duplicateVariableSyntax :: Syntax
duplicateVariableSyntax = NewQVarOp "q" One $ NewQVarOp "q" One NilOp

undeclaredVariableSyntax :: Syntax
undeclaredVariableSyntax = MeasureOp ["q1"] NilOp
