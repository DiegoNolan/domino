{-# LANGUAGE NoImplicitPrelude,
             LambdaCase,
             OverloadedStrings #-}
module Domino.DoubleSix
 ( DoubleSix(..)
 , Section(..)
 , loadGSImage
 , scaleDoubleSix
 ) where

import ClassyPrelude
import Asciify
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

