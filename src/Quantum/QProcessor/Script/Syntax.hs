module Quantum.QProcessor.Script.Syntax
  ( Syntax(..)
  , TransitionType(..)
  ) where

import Quantum.QProcessor

data Syntax =
    NewQVarOp String Bit Syntax
  | TransitionOp TransitionType Syntax
  | MeasureOp [String] Syntax
  | SpyStateOp Syntax
  | SpyProbsOp Syntax
  | NilOp
    deriving (Show, Eq)

data TransitionType =
    Hadamard String
  | PauliX String
  | PauliY String
  | PauliZ String
  | Phase Double String
  | Not String
  | Control String TransitionType
    deriving (Show, Eq)
