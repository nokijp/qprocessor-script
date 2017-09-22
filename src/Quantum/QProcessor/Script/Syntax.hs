module Quantum.QProcessor.Script.Syntax
  ( Syntax(..)
  , TransitionType(..)
  ) where

import Quantum.QProcessor

data Syntax =
    NilOp
  | NewQVarOp String Bit Syntax
  | TransitionOp TransitionType Syntax
  | MeasureOp String Syntax
  | SpyStateOp Syntax
  | SpyProbsOp Syntax
  deriving (Show, Eq)

data TransitionType =
    Hadamard String
  | PauliX String
  | PauliY String
  | PauliZ String
  | CNot String String
  | Toffoli String String String
  deriving (Show, Eq)
