module Quantum.QProcessor.Script.DiagramSpec
  ( main
  , spec
  ) where

import Test.Hspec

import Data.List
import Quantum.QProcessor
import Quantum.QProcessor.Script.Diagram

import Control.Monad

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "digram" $ do
    forM_
      [ ("emptyBitElems", emptyBitBits, emptyBitElems, emptyBitOutput)
      , ("emptyLaneElems", emptyLaneBits, emptyLaneElems, emptyLaneOutput)
      , ("singleLaneElems", singleLaneBits, singleLaneElems, singleLaneOutput)
      , ("allOpsElems", allOpsBits, allOpsElems, allOpsOutput)
      , ("measurementElems", measurementBits, measurementElems, measurementOutput)
      , ("collapsingElems", collapsingBits, collapsingElems, collapsingOutput)
      ] $ \(name, bits, elems, output) ->
        it ("can convert " ++ show name ++ " to a diagram") $ diagram bits elems `shouldBe` output

emptyBitBits :: [Bit]
emptyBitBits = []
emptyBitElems :: [DiagramElem]
emptyBitElems = []
emptyBitOutput :: String
emptyBitOutput = ""

emptyLaneBits :: [Bit]
emptyLaneBits = [Zero, Zero]
emptyLaneElems :: [DiagramElem]
emptyLaneElems = []
emptyLaneOutput :: String
emptyLaneOutput = intercalate "\n"
  [ "|0>--"
  , "     "
  , "|0>--"
  ]

singleLaneBits :: [Bit]
singleLaneBits = [Zero]
singleLaneElems :: [DiagramElem]
singleLaneElems =
  [ DiagramTransition "H" [] 0
  , DiagramMeasure [0]
  ]
singleLaneOutput :: String
singleLaneOutput = "|0>--H--@--"

allOpsBits :: [Bit]
allOpsBits = [Zero, Zero, One]
allOpsElems :: [DiagramElem]
allOpsElems =
  [ DiagramTransition "H" [] 0
  , DiagramTransition "X" [] 0
  , DiagramTransition "Y" [] 0
  , DiagramTransition "Z" [] 0
  , DiagramTransition "R(0.5000)" [] 0
  , DiagramTransition "⊕" [0] 1
  , DiagramTransition "⊕" [0, 1] 2
  , DiagramTransition "X" [0] 1
  , DiagramTransition "X" [0, 1] 2
  , DiagramMeasure [0, 2]
  , DiagramMeasure [0, 1, 2]
  ]
allOpsOutput :: String
allOpsOutput = intercalate "\n"
  [ "|0>--H--X--Y--Z--R(0.5000)--*--*--*--*--@--@--"
  , "                            |  |  |  |        "
  , "|0>-------------------------⊕--*--X--*-----@--"
  , "                               |     |        "
  , "|1>----------------------------⊕-----X--@--@--"
  ]

measurementBits :: [Bit]
measurementBits = [Zero, Zero, Zero]
measurementElems :: [DiagramElem]
measurementElems =
  [ DiagramMeasure [0]
  , DiagramMeasure [1]
  , DiagramMeasure [2]
  , DiagramMeasure [0, 1]
  , DiagramMeasure [2]
  ]
measurementOutput :: String
measurementOutput = intercalate "\n"
  [ "|0>--@--------@-----"
  , "                    "
  , "|0>-----@-----@-----"
  , "                    "
  , "|0>--------@-----@--"
  ]

collapsingBits :: [Bit]
collapsingBits = [Zero, One, Zero, One]
collapsingElems :: [DiagramElem]
collapsingElems =
  [ DiagramTransition "H" [] 0
  , DiagramTransition "H" [] 1
  , DiagramTransition "X" [] 0
  , DiagramTransition "⊕" [0] 1
  , DiagramTransition "H" [] 2
  , DiagramTransition "X" [] 2
  , DiagramTransition "H" [] 3
  , DiagramTransition "⊕" [2] 3
  , DiagramTransition "X" [] 0
  , DiagramTransition "⊕" [1] 2
  , DiagramTransition "⊕" [3] 0
  , DiagramTransition "⊕" [0, 2] 3
  , DiagramTransition "⊕" [0, 3] 2
  , DiagramTransition "Y" [0] 1
  , DiagramTransition "H" [] 0
  , DiagramTransition "Y" [3] 2
  , DiagramTransition "Y" [1] 3
  , DiagramTransition "Z" [] 0
  , DiagramTransition "R(0.5000)" [] 1
  , DiagramTransition "Z" [] 2
  , DiagramTransition "Z" [] 3
  , DiagramTransition "R(0.5000)" [1] 0
  , DiagramTransition "Z" [] 2
  , DiagramTransition "Z" [] 0
  , DiagramTransition "R(0.5000)" [2] 1
  , DiagramMeasure [3]
  , DiagramMeasure [2, 3]
  ]
collapsingOutput :: String
collapsingOutput = intercalate "\n"
  [ "|0>--H--X--*--X--⊕--*--*--*--H------Z------R(0.5000)------Z---------"
  , "           |     |  |  |  |                    |                    "
  , "|1>--H-----⊕--*--|--|--|--Y--*--R(0.5000)------*------R(0.5000)-----"
  , "              |  |  |  |     |                            |         "
  , "|0>--H--X--*--⊕--|--*--⊕--Y--|------Z----------Z----------*------@--"
  , "           |     |  |  |  |  |                                      "
  , "|1>--H-----⊕-----*--⊕--*--*--Y------Z----------@-----------------@--"
  ]
