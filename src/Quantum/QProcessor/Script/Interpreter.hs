{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules, TemplateHaskell #-}

module Quantum.QProcessor.Script.Interpreter
  ( interpret
  , interpretList
  , unsafeInterpretIO
  , unsafeInterpretList
  ) where

import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.RWS
import Data.Complex hiding (phase)
import Data.List
import Data.Maybe
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Text.InterpolatedString.Perl6 (qc)
import Text.Printf
import Quantum.QProcessor
import Quantum.QProcessor.Gate
import Quantum.QProcessor.Manipulator
import Quantum.QProcessor.Script.Diagram
import Quantum.QProcessor.Script.Syntax

data IState = IState { _declaredQVars :: Map String QVar, _initialBits :: [Bit], _diagramElems :: [DiagramElem] }
makeLenses ''IState

type RWSMManipulator w = RWST ([Bit] -> w, [Coef] -> w, [Double] -> w, [Bit] -> [DiagramElem] -> w) w IState (MaybeT Manipulator)

interpret :: Monoid w => ([Bit] -> w, [Coef] -> w, [Double] -> w, [Bit] -> [DiagramElem] -> w) -> Syntax -> IO (Maybe w)
interpret fs = runManipulator . toManipulator fs

interpretList :: Syntax -> IO (Maybe [String])
interpretList = interpret ((:[]) . bitToString, (:[]) . stateToString, (:[]) . probsToString, \bs es -> [diagram bs es])
  where
    bitToString bs = "measure: " ++ foldMap show bs
    stateToString ss = "state: " ++ intercalate " + " (zipWith (\n s -> [qc|({complexToString s})|{n :: Int}>|]) [0..] ss)
    probsToString ps = "probs: " ++ intercalate ", " (zipWith (\n p -> [qc||{n :: Int}> {roundStr p}|]) [0..] ps)
    complexToString (x :+ y) = [qc|{roundStr x} {sign y} {roundStr (abs y)}i|] :: String
    sign x = if x >= 0 then "+" else "-"

unsafeInterpretIO :: Syntax -> IO ()
unsafeInterpretIO s = interpretList s >>= maybe (fail "invalid syntax") (mapM_ putStrLn)

unsafeInterpretList :: Syntax -> IO [String]
unsafeInterpretList s = fromMaybe (fail "invalid syntax") <$> interpretList s

toManipulator :: Monoid w => ([Bit] -> w, [Coef] -> w, [Double] -> w, [Bit] -> [DiagramElem] -> w) -> Syntax -> Manipulator (Maybe w)
toManipulator fs s = runMaybeT $ snd <$> evalRWST (toManipulator' s) fs (IState M.empty [] [])

toManipulator' :: Monoid w => Syntax -> RWSMManipulator w ()
toManipulator' (NewQVarOp n b k) = do
  q <- lift $ lift $ newQVar b
  iState <- get
  lift $ guard (M.notMember n (iState ^. declaredQVars))
  put $ iState & declaredQVars %~ M.insert n q & initialBits %~ (b:)
  toManipulator' k
toManipulator' (TransitionOp tt k) = do
  t <- toTransition tt
  lift $ lift $ transition t
  pushDiagramTransition tt
  toManipulator' k
toManipulator' (MeasureOp ns k) = do
  qs <- mapM getQVar ns
  bs <- lift $ lift $ mapM measure qs
  (bitsToW, _, _, _) <- ask
  tell $ bitsToW bs
  modify $ \iState -> iState & diagramElems %~ (DiagramMeasure (qVarToIndex <$> qs) :)
  toManipulator' k
toManipulator' (SpyStateOp k) = do
  ss <- lift $ lift spyState
  (_, stateToW, _, _) <- ask
  tell $ stateToW ss
  toManipulator' k
toManipulator' (SpyProbsOp k) = do
  ps <- lift $ lift spyProbs
  (_, _, probsToW, _) <- ask
  tell $ probsToW ps
  toManipulator' k
toManipulator' (DiagramOp k) = do
  iState <- get
  (_, _, _, diagramElemsToW) <- ask
  tell $ diagramElemsToW (reverse $ iState ^. initialBits) (reverse $ iState ^. diagramElems)
  toManipulator' k
toManipulator' NilOp = return ()

toTransition :: Monoid w => TransitionType -> RWSMManipulator w Transition
toTransition (Hadamard n) = hadamard <$> getQVar n
toTransition (PauliX n) = pauliX <$> getQVar n
toTransition (PauliY n) = pauliY <$> getQVar n
toTransition (PauliZ n) = pauliZ <$> getQVar n
toTransition (Phase t n) = phase t <$> getQVar n
toTransition (Not n) = pauliX <$> getQVar n
toTransition (Control n tt) = control <$> getQVar n <*> toTransition tt

pushDiagramTransition :: Monoid w => TransitionType -> RWSMManipulator w ()
pushDiagramTransition tt = modify $ \iState -> iState & diagramElems %~ (toDiagramTransition (iState ^. declaredQVars) [] tt :)
  where
    toDiagramTransition :: Map String QVar -> [Int] -> TransitionType -> DiagramElem
    toDiagramTransition vars cs (Hadamard n) = simpleTransition vars cs n "H"
    toDiagramTransition vars cs (PauliX n) = simpleTransition vars cs n "X"
    toDiagramTransition vars cs (PauliY n) = simpleTransition vars cs n "Y"
    toDiagramTransition vars cs (PauliZ n) = simpleTransition vars cs n "Z"
    toDiagramTransition vars cs (Phase t n) = simpleTransition vars cs n $ "R(" ++ roundStr t ++ ")"
    toDiagramTransition vars cs (Not n) = simpleTransition vars cs n "⊕"
    toDiagramTransition vars cs (Control n k) = toDiagramTransition vars (varIndex vars n : cs) k
    simpleTransition vars cs n sign = DiagramTransition sign cs (varIndex vars n)
    varIndex vars = qVarToIndex . (vars M.!)

getQVar :: Monoid w => String -> RWSMManipulator w QVar
getQVar n = do
  iState <- get
  lift $ MaybeT $ return $ M.lookup n (iState ^. declaredQVars)

qVarToIndex :: QVar -> Int
qVarToIndex (QVar n) = n

roundStr :: Double -> String
roundStr = printf "%0.4f"
