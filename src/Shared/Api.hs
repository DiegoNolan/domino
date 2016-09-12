{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TemplateHaskell   #-}
module Shared.Api where

import ClassyPrelude
import Servant.API
import Shared.Domino.DoubleSix.Stats

type Api =
       "new-image" :> QueryParam "url" Text :> Post '[JSON] Int
  :<|> "get-stats" :> Capture "image-id" Int :> Get '[JSON] Stats
