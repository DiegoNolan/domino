{-# LANGUAGE LambdaCase,
             OverloadedStrings,
             TemplateHaskell #-}
module Shared.Domino.DoubleSix
 ( DoubleSix(..)
 , Section(..)
 ) where

import Data.Aeson
import Data.Aeson.TH

data DoubleSix = DoubleSix
  { left :: Section
  , right :: Section
  } deriving (Eq, Show, Read, Ord)

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
