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
import Data.Complex hiding (phase)
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Text.InterpolatedString.Perl6 (qc)
import Text.Printf
import Quantum.QProcessor
import Quantum.QProcessor.Gate
import Quantum.QProcessor.Manipulator
import Quantum.QProcessor.Script.Syntax

type RWSMManipulator w = RWST ([Bit] -> w, [Coef] -> w, [Double] -> w) w (Map String QVar) (MaybeT Manipulator)

interpret :: Monoid w => ([Bit] -> w) -> ([Coef] -> w) -> ([Double] -> w) -> Syntax -> IO (Maybe w)
interpret bitsToW stateToW probsToW = runManipulator . toManipulator bitsToW stateToW probsToW

interpretIO :: Syntax -> IO ()
interpretIO s = interpretList s >>= maybe (fail "invalid syntax") (mapM_ putStrLn)

interpretList :: Syntax -> IO (Maybe [String])
interpretList = interpret ((:[]) . bitToString) ((:[]) . stateToString) ((:[]) . probsToString)
  where
    bitToString bs = "measure: " ++ foldMap show bs
    stateToString ss = "state: " ++ intercalate " + " (zipWith (\n s -> [qc|({complexToString s})|{n :: Int}>|]) [0..] ss)
    probsToString ps = "probs: " ++ intercalate ", " (zipWith (\n p -> [qc||{n :: Int}> {roundStr p}|]) [0..] ps)
    complexToString (x :+ y) = [qc|{roundStr x} {sign y} {roundStr (abs y)}i|] :: String
    sign x = if x >= 0 then "+" else "-"
    roundStr x = printf "%0.4f" x :: String

toManipulator :: Monoid w => ([Bit] -> w) -> ([Coef] -> w) -> ([Double] -> w) -> Syntax -> Manipulator (Maybe w)
toManipulator bitsToW stateToW probsToW s =
  runMaybeT $ snd <$> evalRWST (toManipulator' s) (bitsToW, stateToW, probsToW) M.empty

toManipulator' :: Monoid w => Syntax -> RWSMManipulator w ()
toManipulator' (NewQVarOp n b k) = do
  q <- lift $ lift $ newQVar b
  vars <- get
  lift $ guard (M.notMember n vars)
  put $ M.insert n q vars
  toManipulator' k
toManipulator' (TransitionOp tt k) = do
  t <- toTransition tt
  lift $ lift $ transition t
  toManipulator' k
toManipulator' (MeasureOp ns k) = do
  qs <- mapM getQVar ns
  bs <- lift $ lift $ mapM measure qs
  (bitsToW, _, _) <- ask
  tell $ bitsToW bs
  toManipulator' k
toManipulator' (SpyStateOp k) = do
  ss <- lift $ lift spyState
  (_, stateToW, _) <- ask
  tell $ stateToW ss
  toManipulator' k
toManipulator' (SpyProbsOp k) = do
  ps <- lift $ lift spyProbs
  (_, _, probsToW) <- ask
  tell $ probsToW ps
  toManipulator' k
toManipulator' NilOp = return ()

toTransition :: Monoid w => TransitionType -> RWSMManipulator w Transition
toTransition (Hadamard n) = hadamard <$> getQVar n
toTransition (PauliX n) = pauliX <$> getQVar n
toTransition (PauliY n) = pauliY <$> getQVar n
toTransition (PauliZ n) = pauliZ <$> getQVar n
toTransition (Phase t n) = phase t <$> getQVar n
toTransition (Control n tt) = control <$> getQVar n <*> toTransition tt

getQVar :: Monoid w => String -> RWSMManipulator w QVar
getQVar n = get >>= lift . MaybeT . return . M.lookup n
