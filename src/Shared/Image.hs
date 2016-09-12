{-# LANGUAGE NoImplicitPrelude,
             OverloadedStrings #-}
module Shared.Image where

import ClassyPrelude

imageUrl :: Int -> String
imageUrl imgId = "https://s3-us-west-2.amazonaws.com/domino/" ++ show imgId


