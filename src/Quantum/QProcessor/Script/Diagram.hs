module Quantum.QProcessor.Script.Diagram
  ( DiagramElem(..)
  , diagram
  ) where

import Control.Monad
import Data.List
import Data.Maybe
import Data.Vector (Vector)
import qualified Data.Vector as V
import Quantum.QProcessor

type VV a = Vector (Vector a)

data DiagramElem = DiagramTransition String [Int] Int | DiagramMeasure [Int]

diagram :: [Bit] -> [DiagramElem] -> String
diagram bs es = intercalate "\n" $ takeLane laneStringsWithBits gapLaneStringsWithBits
  where
    bitNum = length bs
    (lanes, gapLanes) = diagramLanes (V.replicate bitNum V.empty) (V.replicate (bitNum - 1) V.empty) 0 es
    widths = signWidths lanes
    laneStrings = V.toList $ laneString '-' widths <$> lanes
    gapLaneStrings = V.toList $ laneString ' ' widths <$> gapLanes
    laneStringsWithBits = zipWith (\b s -> show b ++ s) bs laneStrings
    gapLaneStringsWithBits = ("   " ++) <$> gapLaneStrings
    takeLane [] _ = []
    takeLane (l:ls) gs = l : takeGap ls gs
    takeGap _ [] = []
    takeGap ls (g:gs) = g : takeLane ls gs

signWidths :: VV (Maybe String) -> Vector Int
signWidths lanes =
  let
    laneLength = V.maximum $ V.length <$> lanes
  in
    V.generate laneLength $ \signIndex ->
      V.maximum $ V.generate (V.length lanes) $ \laneIndex ->
        maybe 0 length $ join $ (lanes V.! laneIndex) V.!? signIndex

laneString :: Char -> Vector Int -> Vector (Maybe String) -> String
laneString emptySign widths lane = connector ++ foldr (++) "" signStrs
  where
    connector = replicate 2 emptySign
    signStrs = flip V.imap widths $ \i width ->
      let
        sign = fromMaybe [] $ join $ lane V.!? i
        emptyWidth = width - length sign
        rightEmptyWidth = emptyWidth `div` 2
        leftEmptyWidth = emptyWidth - rightEmptyWidth
      in replicate leftEmptyWidth emptySign ++ sign ++ replicate rightEmptyWidth emptySign ++ connector

diagramLanes :: VV (Maybe String) -> VV (Maybe String) -> Int -> [DiagramElem] -> (VV (Maybe String), VV (Maybe String))
diagramLanes lanes gapLanes _ [] = (lanes, gapLanes)
diagramLanes lanes gapLanes measurePos (DiagramTransition sign cs t : es) = diagramLanes newLanes newGapLanes measurePos es
  where
    minLane = minimum (t:cs)
    maxLane = maximum (t:cs)
    pos = maximum $ (V.length . (lanes V.!)) <$> [minLane..maxLane]
    signStr i
      | i == t = sign
      | i `elem` cs = "*"
      | otherwise = "|"
    newLanes = flip V.imap lanes $ \i lane ->
      if i >= minLane && i <= maxLane
      then lane V.++ V.replicate (pos - V.length lane) Nothing `V.snoc` Just (signStr i)
      else lane
    newGapLanes = flip V.imap gapLanes $ \i gapLane ->
      if i >= minLane && i < maxLane
      then gapLane V.++ V.replicate (pos - V.length gapLane) Nothing `V.snoc` Just "|"
      else gapLane
diagramLanes lanes gapLanes measurePos (DiagramMeasure ts : es) = diagramLanes newLanes gapLanes (measurePos + 1) es
  where
    minLane = minimum ts
    maxLane = maximum ts
    pos = measurePos `max` maximum ((V.length . (lanes V.!)) <$> [minLane..maxLane])
    newLanes = flip V.imap lanes $ \i lane ->
      if i `elem` ts
      then lane V.++ V.replicate (pos - V.length lane) Nothing `V.snoc` Just "@"
      else lane
