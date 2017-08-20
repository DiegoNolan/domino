{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE RankNTypes,
             FlexibleContexts,
             OverloadedStrings,
             RecursiveDo,
             PartialTypeSignatures,
             MultiWayIf,
             RecordWildCards,
             TypeFamilies,
             NoMonomorphismRestriction #-}
module ClientAPI where

import Data.Proxy
import Reflex
import Reflex.Dom
import Servant.API
import Servant.Reflex
import Shared.Api

type API = "test" :> Get '[JSON] Int


api :: Proxy API
api = Proxy


(      newImage
  :<|> getArtRows
  ) = client (Proxy :: Proxy Api)
             (Proxy :: MonadWidget t m => Proxy m)
             (Proxy :: Proxy ())
             (constDyn (BasePath "/api"))


