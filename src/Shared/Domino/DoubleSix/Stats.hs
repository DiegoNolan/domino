{-# LANGUAGE LambdaCase,
             OverloadedStrings,
             RecordWildCards,
             TemplateHaskell #-}
module Shared.Domino.DoubleSix.Stats
 ( Stats(..)
 , getStats
 , desiredWidthToCount
 ) where

import Data.Aeson
import Data.Aeson.TH
import Data.Char
import Data.List
import Shared.Domino.DoubleSix
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

desiredWidthToCount :: Double -> Int
desiredWidthToCount w = round $ w / dominoWidth

-- In inches
dominoWidth :: Double
dominoWidth = 1.5

dominoHeight :: Double
dominoHeight = 0.75

pricePerDomino :: Rational
pricePerDomino = 17 / (28*12)

weightPerDomino :: Double
weightPerDomino = 0.07142 / 16

margins :: Double
margins = 100

data Stats = Stats
  { totalDominoes :: Int
  , dominoCounts :: [(DoubleSix, Int)]
  , tileDimensions :: (Int, Int)
  , physicalDimensions :: (Double, Double)
  , weight :: Double
  , price :: Int
  }

instance Pretty Stats where
  pPrint Stats{..} =
      vcat [ text "Total Dominoes: " <> int totalDominoes
           , text "Weight: " <> double weight <> text " lbs"
           , text "Price: $" <> int (price `div` 100)
           , text "Dimensions: " <> double (w / 12) <> text "' x " <>
                                    double (h / 12) <> text "'"
           , text "Tiles: " <> int (fst tileDimensions) <> text "x"
                            <> int (snd tileDimensions)
           , text "Domino Counts:"
           , counts
           ]
    where
        (w, h) = (fst physicalDimensions, snd physicalDimensions)
        counts = vcat $ map (\c -> nest 3 $ doubleSixAndCount c) dominoCounts
        doubleSixAndCount (ds, cnt) = int cnt <> text " _ " <> dsToDoc ds
        dsToDoc :: DoubleSix -> Doc
        dsToDoc ds = toChar mn
          where
            mn = makeMinDoubleSix ds
            toChar (DoubleSix Blank Blank) = text "0 | 0"
            toChar (DoubleSix Blank One) = text "0 | 1"
            toChar (DoubleSix Blank Two) = text "0 | 2"
            toChar (DoubleSix Blank Three) = text "0 | 3"
            toChar (DoubleSix Blank Four) = text "0 | 4"
            toChar (DoubleSix Blank Five) = text "0 | 5"
            toChar (DoubleSix Blank Six) = text "0 | 6"
            toChar (DoubleSix One One) = text "1 | 1"
            toChar (DoubleSix One Two) = text "1 | 2"
            toChar (DoubleSix One Three) = text "1 | 3"
            toChar (DoubleSix One Four) = text "1 | 4"
            toChar (DoubleSix One Five) = text "1 | 5"
            toChar (DoubleSix One Six) = text "1 | 6"
            toChar (DoubleSix Two Two) = text "2 | 2"
            toChar (DoubleSix Two Three) = text "2 | 3"
            toChar (DoubleSix Two Four) = text "2 | 4"
            toChar (DoubleSix Two Five) = text "2 | 5"
            toChar (DoubleSix Two Six) = text "2 | 6"
            toChar (DoubleSix Three Three) = text "3 | 3"
            toChar (DoubleSix Three Four) = text "3 | 4"
            toChar (DoubleSix Three Five) = text "3 | 5"
            toChar (DoubleSix Three Six) = text "3 | 6"
            toChar (DoubleSix Four Four) = text "4 | 4"
            toChar (DoubleSix Four Five) = text "4 | 5"
            toChar (DoubleSix Four Six) = text "4 | 6"
            toChar (DoubleSix Five Five) = text "5 | 5"
            toChar (DoubleSix Five Six) = text "5 | 6"
            toChar (DoubleSix Six Six) = text "6 | 6"
            toChar _ = error "doubleSixToChar should be impossible"

doubleSixToChar :: DoubleSix -> Char
doubleSixToChar ds = toChar mn
  where
    mn = makeMinDoubleSix ds
    toChar (DoubleSix Blank Blank) = chr 127025
    toChar (DoubleSix Blank One) = chr 127026
    toChar (DoubleSix Blank Two) = chr 127027
    toChar (DoubleSix Blank Three) = chr 127028
    toChar (DoubleSix Blank Four) = chr 127029
    toChar (DoubleSix Blank Five) = chr 127030
    toChar (DoubleSix Blank Six) = chr 127031
    toChar (DoubleSix One One) = chr 127033
    toChar (DoubleSix One Two) = chr 127034
    toChar (DoubleSix One Three) = chr 127035
    toChar (DoubleSix One Four) = chr 127036
    toChar (DoubleSix One Five) = chr 127037
    toChar (DoubleSix One Six) = chr 127038
    toChar (DoubleSix Two Two) = chr 127041
    toChar (DoubleSix Two Three) = chr 127042
    toChar (DoubleSix Two Four) = chr 127043
    toChar (DoubleSix Two Five) = chr 127044
    toChar (DoubleSix Two Six) = chr 127045
    toChar (DoubleSix Three Three) = chr 127049
    toChar (DoubleSix Three Four) = chr 127050
    toChar (DoubleSix Three Five) = chr 127051
    toChar (DoubleSix Three Six) = chr 127052
    toChar (DoubleSix Four Four) = chr 127057
    toChar (DoubleSix Four Five) = chr 127058
    toChar (DoubleSix Four Six) = chr 127059
    toChar (DoubleSix Five Five) = chr 127065
    toChar (DoubleSix Five Six) = chr 127066
    toChar (DoubleSix Six Six) = chr 127073
    toChar _ = error "doubleSixToChar should be impossible"


getStats :: [[DoubleSix]] -> Stats
getStats rows =
    Stats
      { totalDominoes = t
      , dominoCounts = cnts
      , tileDimensions = (w,h)
      , physicalDimensions = (dominoWidth * fromIntegral w, dominoHeight * fromIntegral h)
      , weight = weightPerDomino * fromIntegral t
      , price = ceiling $ pricePerDomino * 100 * fromIntegral t
      }
  where
    t = sum $ map snd cnts
    cnts = getCounts $ concat rows
    -- TODO: unsafe
    w = length (head rows)
    h = length rows

getCounts :: [DoubleSix] -> [(DoubleSix, Int)]
getCounts doms = map (\lst@(x:_) -> (x, length lst)) grouped
  where
    order (DoubleSix a b) = if a < b then DoubleSix a b else DoubleSix b a
    grouped = group $ sort (map order doms)

$(deriveJSON defaultOptions ''Stats)
