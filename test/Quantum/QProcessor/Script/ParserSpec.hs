{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}

module Quantum.QProcessor.Script.ParserSpec
  ( main
  , spec
  ) where

import Test.Hspec
import Test.Hspec.Expectations.Contrib

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
      , ("allOpsInput", allOpsInput, allOpsSyntax)
      ] $ \(name, input, syntax) ->
        it ("should accept " ++ name) $ runScriptParser (parser <* eof) input `shouldBe` Right syntax
    forM_
      [ ("multipleOperationInput", multipleOperationInput)
      , ("duplicateVariableInput", duplicateVariableInput)
      , ("undeclaredVariableInput1", undeclaredVariableInput1)
      , ("undeclaredVariableInput2", undeclaredVariableInput2)
      ] $ \(name, input) ->
        it ("should return an error when given " ++ name) $ isLeft $ runScriptParser (parser <* eof) input

emptyInput :: String
emptyInput = [q||]

emptySyntax :: Syntax
emptySyntax = NilOp

smallInput :: String
smallInput = [q|q = newBit 0|]

smallSyntax :: Syntax
smallSyntax = NewQVarOp "q" Zero NilOp

delimiterInput1 :: String
delimiterInput1 = [q|  # comment


q0 = newBit 0  # comment

  q1= newBit 1# comment
q2 =  newBit 0#

#comment|]

delimiterSyntax1 :: Syntax
delimiterSyntax1 = NewQVarOp "q0" Zero $ NewQVarOp "q1" One $ NewQVarOp "q2" Zero NilOp

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
delimiterInput4 = [q|# comment|]

delimiterSyntax4 :: Syntax
delimiterSyntax4 = NilOp

allOpsInput :: String
allOpsInput = [q|
q0 = newBit 0
q1 = newBit 0
q2 = newBit 1
H q0
X q0
Y q0
Z q0
R 0.5 q0
CNOT q0 q1
CCNOT q0 q1 q2
control q0 (X q1)
control q0 (control q1 (X q2))
measure q0
measure q0 q1 q2
spyState
spyProbs
spyProbs #
spyProbs q0
spyProbs q0 q1
diagram
|]

allOpsSyntax :: Syntax
allOpsSyntax =
    NewQVarOp "q0" Zero
  $ NewQVarOp "q1" Zero
  $ NewQVarOp "q2" One
  $ TransitionOp (Hadamard "q0")
  $ TransitionOp (PauliX "q0")
  $ TransitionOp (PauliY "q0")
  $ TransitionOp (PauliZ "q0")
  $ TransitionOp (Phase 0.5 "q0")
  $ TransitionOp (Control "q0" $ Not "q1")
  $ TransitionOp (Control "q0" $ Control "q1" $ Not "q2")
  $ TransitionOp (Control "q0" $ PauliX "q1")
  $ TransitionOp (Control "q0" $ Control "q1" $ PauliX "q2")
  $ MeasureOp ["q0"]
  $ MeasureOp ["q0", "q1", "q2"]
  $ SpyStateOp
  $ SpyProbsOp
  $ SpyProbsOp
  $ SpyProbsPartialOp ["q0"]
  $ SpyProbsPartialOp ["q0", "q1"]
  $ DiagramOp
  $ NilOp

multipleOperationInput :: String
multipleOperationInput = [q|spyState spyState|]

duplicateVariableInput :: String
duplicateVariableInput = [q|
q = newBit 0
measure q
q = newBit 0
|]

undeclaredVariableInput1 :: String
undeclaredVariableInput1 = [q|measure q|]

undeclaredVariableInput2 :: String
undeclaredVariableInput2 = [q|spyProbs q|]
