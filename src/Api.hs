{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TemplateHaskell   #-}
module Api where

import Control.Lens
import ClassyPrelude
import Domino.DoubleSix.Stats
import Servant

type Api m =
       "new-image" :> QueryParam "url" Text :> Post '[JSON] Int
  :<|> "get-stats" :> QueryParam "image-id" Int :> Post '[JSON] Stats
