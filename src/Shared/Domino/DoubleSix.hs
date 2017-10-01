{-# LANGUAGE LambdaCase,
             OverloadedStrings,
             RecordWildCards,
             TemplateHaskell #-}
module Shared.Domino.DoubleSix
 ( DoubleSix(..)
 , Section(..)
 , flipDomino
 , makeMinDoubleSix
 ) where

import Data.Aeson
import Data.Aeson.TH

data DoubleSix = DoubleSix
  { left :: Section
  , right :: Section
  } deriving (Eq, Show, Read, Ord)

flipDomino :: DoubleSix -> DoubleSix
flipDomino DoubleSix{..} = DoubleSix right left

makeMinDoubleSix :: DoubleSix -> DoubleSix
makeMinDoubleSix ds = if flipDomino ds < ds then flipDomino ds else ds

data Section =
    Blank
  | One
  | Two
  | Three
  | Four
  | Five
  | Six deriving (Eq, Show, Read, Ord)


$(deriveJSON defaultOptions ''Section)
$(deriveJSON defaultOptions ''DoubleSix)
