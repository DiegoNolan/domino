{-# LANGUAGE NoImplicitPrelude,
             LambdaCase,
             OverloadedStrings #-}
module Domino.DoubleSix.Stats
 ( Stats(..)
 , getStats
 ) where

import ClassyPrelude
import Domino.DoubleSix

-- In inches
dominoWidth :: Double
dominoWidth = 1.5

dominoHeight :: Double
dominoHeight = 0.75

pricePerDomino :: Rational
pricePerDomino = 17 / 28

weightPerDomino :: Double
weightPerDomino = 0.07142

margins :: Double
margins = 100

data Stats = Stats
  { dominoRows :: [[DoubleSix]]
  , totalDominoes :: Int
  , dominoCounts :: [(DoubleSix, Int)]
  , tileDimensions :: (Int, Int)
  , physicalDimensions :: (Double, Double)
  , weight :: Double
  , price :: Int
  }

getStats :: [[DoubleSix]] -> Stats
getStats rows =
    Stats
      { dominoRows = rows
      , totalDominoes = t
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
    w = length (headEx rows)
    h = length rows

getCounts :: [DoubleSix] -> [(DoubleSix, Int)]
getCounts doms = map (\lst@(x:_) -> (x, length lst)) grouped
  where
    order (DoubleSix a b) = if a < b then DoubleSix a b else DoubleSix b a
    grouped = group $ sort (map order doms)
