{-# LANGUAGE FlexibleInstances,
             NoImplicitPrelude,
             OverloadedStrings #-}
module Snap.Snaplet.AWS
  ( putFile
  , getFile
  , sendMail
  ) where

import ClassyPrelude
import Control.Lens
import Control.Monad.Representable.State (get)
import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Conduit.Binary
import Network.AWS
import Network.AWS.Data.Body
import qualified Network.AWS.S3 as S3
import qualified Network.AWS.SES as SES

getEnv :: IO Env
getEnv = newEnv Oregon Discover

sendMail :: Text -- ^ From
         -> [Text] -- ^ To
         -> Text -- ^ subject
         -> Text -- ^ body
         -> IO ()
sendMail from to subject b = do
  let dest = SES.destination & SES.dToAddresses .~ to
      bdy = SES.body & SES.bHTML .~ Just (SES.content b)
      msg = SES.message (SES.content subject) bdy
      email = SES.sendEmail from dest msg
  env <- getEnv
  runResourceT . runAWS env $ void $ send email

putFile :: ToBody a => S3.BucketName -> S3.ObjectKey -> Text -> a -> IO ()
putFile bucketName key contentType file = do
  let poRe = S3.putObject bucketName key (toBody file)
  env <- getEnv
  runResourceT . runAWS env $
    void $ send (poRe & S3.poContentType .~ Just contentType)

getFile :: S3.BucketName -> S3.ObjectKey -> IO LByteString
getFile bucketName key = do
  let goRe = S3.getObject bucketName key
  env <- getEnv
  runResourceT $ do
    resp <- runAWS env $ send goRe
    (source, finalizer) <- unwrapResumable $ _streamBody (resp ^. S3.gorsBody)
    lbs <- source $$ sinkLbs
    finalizer
    return lbs
