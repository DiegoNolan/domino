{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TemplateHaskell   #-}
module Shared.Api where

import Data.Text
import Lucid hiding (with)
import Servant.API
import Servant.HTML.Lucid
import Shared.Domino.DoubleSix

data IndexPage = IndexPage

instance ToHtml IndexPage where
  toHtml :: Monad m => IndexPage -> HtmlT m ()
  toHtml _ =
    masterPage $ do
      div_ [ id_ "create" ] ""
      script_ [ src_ "/js/create.js" ] ""
  toHtmlRaw = toHtml

type Api =
       "newImage" :> QueryParam "url" Text :> Post '[JSON] Int
  :<|> "getArtRows" :> QueryParam "image-id" Int
                    :> QueryParam "desired-width" Double :> Get '[JSON] [[DoubleSix]]

type WrapperApi =
       Get '[HTML] IndexPage
  :<|> "api" :> Api

type StaticApi =
       "js" :> Raw
  :<|> "images" :> Raw

type WholeApi =
       StaticApi
  :<|> WrapperApi

masterPage :: Monad m => HtmlT m () -> HtmlT m ()
masterPage inner =
  html_ $ do
    head_ $ do
      link_ [ href_ "https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/3.3.7/css/bootstrap.css"
            , rel_ "stylesheet"
            ]
    body_ $ do
      inner
