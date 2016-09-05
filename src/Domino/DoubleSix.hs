{-# LANGUAGE NoImplicitPrelude,
             LambdaCase,
             OverloadedStrings #-}
module Domino.DoubleSix
 ( DoubleSix(..)
 , Section(..)
 , loadGSImage
 , scaleDoubleSix
 , novemDoubleSix
 ) where

import ClassyPrelude
import Asciify hiding (Blank)
import qualified Asciify as A
import Codec.Picture

data DoubleSix = DoubleSix
  { left :: Section
  , right :: Section
  } deriving (Eq, Show, Ord)

data Section =
    Blank
  | One
  | Two
  | Three
  | Four
  | Five
  | Six deriving (Eq, Show, Ord)


scaleDoubleSix :: Image Pixel8 -> Int -> [[DoubleSix]]
scaleDoubleSix img chsWide = scaleAsciify' img chsWide scaleMapping

scaleMapping :: Word8 -> DoubleSix
scaleMapping n
  | n >= 240      = DoubleSix Six Six
  | n >= 220      = DoubleSix Six Five
  | n >= 200      = DoubleSix Five Five
  | n >= 180      = DoubleSix Five Four
  | n >= 160      = DoubleSix Four Four
  | n >= 140      = DoubleSix Four Three
  | n >= 120      = DoubleSix Three Three
  | n >= 100      = DoubleSix Three Two
  | n >= 80       = DoubleSix Two Two
  | n >= 60       = DoubleSix Two One
  | n >= 40       = DoubleSix One One
  | n >= 20       = DoubleSix One Blank
  | otherwise     = DoubleSix Blank Blank

novemDoubleSix :: Image Pixel8 -> Int -> [[DoubleSix]]
novemDoubleSix img chsWide = novemAsciify' img chsWide novemMapping

novemMapping :: Novemant -> DoubleSix
novemMapping nvm = DoubleSix (sixMapping leftInt) (sixMapping rightInt)
  where
    maxVal = 3*3 + 3*3 `div` 2
    leftColVal = sum $ map brightNessToInt (leftCol nvm)
    centerColVal = sum $ map brightNessToInt (centerCol nvm)
    rightColVal = sum $ map brightNessToInt (leftCol nvm)
    leftInt = (6 * (leftColVal + centerColVal `div` 2) ) `div` maxVal
    rightInt = (6 * (rightColVal + centerColVal `div` 2) ) `div` maxVal
    sixMapping :: Int -> Section
    sixMapping i = case i of
                     0 -> Blank
                     1 -> One
                     2 -> Two
                     3 -> Three
                     4 -> Four
                     5 -> Five
                     _ -> Six
      where
        clamp = max 0 (min 6 i)

leftCol :: Novemant -> [Brightness]
leftCol (Novemant tl _ _ ml _ _ bl _ _) = [tl, ml, bl]

centerCol :: Novemant -> [Brightness]
centerCol (Novemant _ tc _ _ mc _ _ bc _) = [tc, mc, bc]

rightCol ::  Novemant -> [Brightness]
rightCol (Novemant _ _ rl _ _ mr _ _ br) = [rl, mr, br]

brightNessToInt :: Brightness -> Int
brightNessToInt Bnk = 3
brightNessToInt Lgt = 2
brightNessToInt Drk = 1
brightNessToInt Bck = 0
