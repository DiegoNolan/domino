{-# LANGUAGE ExtendedDefaultRules,
             FlexibleContexts,
             NoImplicitPrelude,
             OverloadedStrings,
             RecordWildCards #-}
module SnapUtil
  ( FromParam (..)
  , getFromParam
  ) where

import ClassyPrelude
import Snap.Core

class FromParam a where
  fromParam :: ByteString -> Maybe a

getFromParam :: (MonadSnap m, FromParam a) => ByteString -> m (Maybe a)
getFromParam name = do
  mp <- getParam name
  return $ mp >>= fromParam
