{-# LANGUAGE NoImplicitPrelude,
             OverloadedStrings,
             TemplateHaskell #-}
module Main where

import ClassyPrelude
import Site
import Snap.Http.Server
import Snap.Snaplet

main :: IO ()
main = serveSnaplet defaultConfig app
