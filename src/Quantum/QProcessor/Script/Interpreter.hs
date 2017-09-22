{-# LANGUAGE FlexibleContexts, QuasiQuotes, ExtendedDefaultRules #-}

module Quantum.QProcessor.Script.Interpreter
  ( interpret
  , interpretIO
  , interpretList
  ) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.RWS
import Data.Complex
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Text.InterpolatedString.Perl6 (qc)
import Text.Printf
import Quantum.QProcessor
import Quantum.QProcessor.Gate
import Quantum.QProcessor.Manipulator
import Quantum.QProcessor.Script.Syntax

type RWSMManipulator w = RWST () w (Map String QVar) (MaybeT Manipulator)

interpret :: Monoid w => (Bit -> w) -> ([Coef] -> w) -> ([Double] -> w) -> Syntax -> IO (Maybe w)
interpret bitToW stateToW probsToW = runManipulator . toManipulator bitToW stateToW probsToW

interpretIO :: Syntax -> IO ()
interpretIO s = interpretList s >>= maybe (fail "invalid syntax") (mapM_ putStrLn)

interpretList :: Syntax -> IO (Maybe [String])
interpretList = interpret ((:[]) . bitToString) ((:[]) . stateToString) ((:[]) . probsToString)
  where
    bitToString b = "measure: " ++ show b
    stateToString ss = "state: " ++ intercalate " + " (zipWith (\n s -> [qc|({complexToString s})|{n :: Int}>|]) [0..] ss)
    probsToString ps = "probs: " ++ intercalate ", " (zipWith (\n p -> [qc||{n :: Int}> {roundStr p}|]) [0..] ps)
    complexToString (x :+ y) = [qc|{roundStr x} {sign y} {roundStr (abs y)}i|] :: String
    sign x = if x >= 0 then "+" else "-"
    roundStr x = printf "%0.4f" x :: String

toManipulator :: Monoid w => (Bit -> w) -> ([Coef] -> w) -> ([Double] -> w) -> Syntax -> Manipulator (Maybe w)
toManipulator bitToW stateToW probsToW s = runMaybeT $ snd <$> evalRWST (toManipulator' bitToW stateToW probsToW s) () M.empty

toManipulator' :: Monoid w => (Bit -> w) -> ([Coef] -> w) -> ([Double] -> w) -> Syntax -> RWSMManipulator w ()
toManipulator' bitToW stateToW probsToW (NewQVarOp n b k) = do
  q <- lift $ lift $ newQVar b
  vars <- get
  lift $ guard (M.notMember n vars)
  put $ M.insert n q vars
  toManipulator' bitToW stateToW probsToW k
toManipulator' bitToW stateToW probsToW (TransitionOp tt k) = do
  t <- toTransition tt
  lift $ lift $ transition t
  toManipulator' bitToW stateToW probsToW k
toManipulator' bitToW stateToW probsToW (MeasureOp n k) = do
  q <- getQVar n
  b <- lift $ lift $ measure q
  tell $ bitToW b
  toManipulator' bitToW stateToW probsToW k
toManipulator' bitToW stateToW probsToW (SpyStateOp k) = do
  ss <- lift $ lift spyState
  tell $ stateToW ss
  toManipulator' bitToW stateToW probsToW k
toManipulator' bitToW stateToW probsToW (SpyProbsOp k) = do
  ps <- lift $ lift spyProbs
  tell $ probsToW ps
  toManipulator' bitToW stateToW probsToW k
toManipulator' _ _ _ NilOp = return ()

toTransition :: Monoid w => TransitionType -> RWSMManipulator w Transition
toTransition (Hadamard n) = hadamard <$> getQVar n
toTransition (PauliX n) = pauliX <$> getQVar n
toTransition (PauliY n) = pauliY <$> getQVar n
toTransition (PauliZ n) = pauliZ <$> getQVar n
toTransition (CNot n1 n2) = cnot <$> getQVar n1 <*> getQVar n2
toTransition (Toffoli n1 n2 n3) = toffoli <$> getQVar n1 <*> getQVar n2 <*> getQVar n3

getQVar :: Monoid w => String -> RWSMManipulator w QVar
getQVar n = get >>= lift . MaybeT . return . M.lookup n
