{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules, TemplateHaskell #-}

module Quantum.QProcessor.Script.Interpreter
  ( InterpreterOutput
  , interpret
  , interpretList
  , unsafeInterpretIO
  , unsafeInterpretList
  ) where

import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.RWS
import Data.Bits
import Data.Complex hiding (phase)
import Data.List
import Data.Maybe
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Text.InterpolatedString.Perl6 (qc)
import Text.Printf
import Quantum.QProcessor
import Quantum.QProcessor.Gate
import Quantum.QProcessor.Manipulator
import Quantum.QProcessor.Script.Diagram
import Quantum.QProcessor.Script.Syntax

data InterpreterOutput =
    MeasureOutput [Bit]
  | SpyStateOutput [Coef]
  | SpyProbsOutput [Double]
  | SpyProbsPartialOutput [Double]
  | DiagramOutput [Bit] [DiagramElem]

data IState = IState { _declaredQVars :: Map String QVar, _initialBits :: [Bit], _diagramElems :: [DiagramElem] }
makeLenses ''IState

type RWSMManipulator w = RWST (InterpreterOutput -> w) w IState (MaybeT Manipulator)

interpret :: Monoid w => (InterpreterOutput -> w) -> Syntax -> IO (Maybe w)
interpret f = runManipulator . toManipulator f

interpretList :: Syntax -> IO (Maybe [String])
interpretList = interpret toMonoid
  where
    toMonoid (MeasureOutput bs)= return $ "measure: " ++ foldMap show bs
    toMonoid (SpyStateOutput ss) = return $ "state: " ++ intercalate " + " (zipWith (\n s -> [qc|({complexToString s})|{n :: Int}>|]) [0..] ss)
    toMonoid (SpyProbsOutput ps) = probsToMonoid ps
    toMonoid (SpyProbsPartialOutput ps) = probsToMonoid ps
    toMonoid (DiagramOutput bs es) = return $ diagram bs es
    probsToMonoid ps = return $ "probs: " ++ intercalate ", " (zipWith (\n p -> [qc||{n :: Int}> {roundStr p}|]) [0..] ps)
    complexToString (x :+ y) = [qc|{roundStr x} {sign y} {roundStr (abs y)}i|] :: String
    sign x = if x >= 0 then "+" else "-"

unsafeInterpretIO :: Syntax -> IO ()
unsafeInterpretIO s = interpretList s >>= maybe (fail "invalid syntax") (mapM_ putStrLn)

unsafeInterpretList :: Syntax -> IO [String]
unsafeInterpretList s = fromMaybe (fail "invalid syntax") <$> interpretList s

toManipulator :: Monoid w => (InterpreterOutput -> w) -> Syntax -> Manipulator (Maybe w)
toManipulator f s = runMaybeT $ snd <$> evalRWST (toManipulator' s) f (IState M.empty [] [])

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
  toMonoid <- ask
  tell $ toMonoid $ MeasureOutput bs
  modify $ \iState -> iState & diagramElems %~ (DiagramMeasure (qVarToIndex <$> qs) :)
  toManipulator' k
toManipulator' (SpyStateOp k) = do
  ss <- lift $ lift spyState
  toMonoid <- ask
  tell $ toMonoid $ SpyStateOutput ss
  toManipulator' k
toManipulator' (SpyProbsOp k) = do
  ps <- lift $ lift spyProbs
  toMonoid <- ask
  tell $ toMonoid $ SpyProbsOutput ps
  toManipulator' k
toManipulator' (SpyProbsPartialOp ns k) = do
  qs <- mapM getQVar ns
  ps <- lift $ lift spyProbs
  toMonoid <- ask
  tell $ toMonoid $ SpyProbsPartialOutput $ marginalize (qVarToIndex <$> qs) ps
  toManipulator' k
toManipulator' (DiagramOp k) = do
  iState <- get
  toMonoid <- ask
  tell $ toMonoid $ DiagramOutput (reverse $ iState ^. initialBits) (reverse $ iState ^. diagramElems)
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

marginalize :: [Int] -> [Double] -> [Double]
marginalize bitIndices probs = V.toList $ V.create $ do
    v <- MV.replicate (1 `shift` length bitIndices) 0
    forM_ (zip [0..] probs) $ \(n, p) -> do
      let shrinkedIndex = sum $ zipWith (\sourceBitIndex targetBitIndex -> ((n `shiftR` sourceBitIndex) .&. 1) `shiftL` targetBitIndex) bitIndices [0..]
      MV.modify v (+p) shrinkedIndex
    return v

roundStr :: Double -> String
roundStr = printf "%0.4f"
