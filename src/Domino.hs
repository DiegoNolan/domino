{-# LANGUAGE NoImplicitPrelude,
             LambdaCase,
             OverloadedStrings #-}
module Domino
 ( loadGSImage
 , novemAsciify
 , novemAsciify'
 , quadAsciify
 , quadAsciify'
 , scaleAsciify
 , scaleAsciify'
 , testAllAlgorithms
 , asciifyCells
 , Cell (..)
 ) where

import ClassyPrelude
import Asciify
import Codec.Picture
import Codec.Picture.Types


scaleDomino :: Image Pixel8 -> Int -> Cell Domino
