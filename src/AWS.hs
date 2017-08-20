{-# LANGUAGE FlexibleInstances,
             NoImplicitPrelude,
             OverloadedStrings #-}
module AWS
  ( HasAWS (..)
  , putFile
  , getFile
  , sendMail
  ) where

import ClassyPrelude
import Control.Lens
import Data.Conduit
import Data.Conduit.Binary
import Network.AWS
import Network.AWS.Data.Body
import qualified Network.AWS.S3 as S3
import qualified Network.AWS.SES as SES

getEnv :: IO Env
getEnv = do
  env <- newEnv Discover
  return $ env & envRegion .~ Oregon

class MonadIO m => HasAWS m where
  getAWSEnv :: m Env

sendMail :: HasAWS m
         => Text -- ^ From
         -> [Text] -- ^ To
         -> Text -- ^ subject
         -> Text -- ^ body
         -> m ()
sendMail from to subject b = do
  env <- getAWSEnv
  let dest = SES.destination & SES.dToAddresses .~ to
      bdy = SES.body & SES.bHTML .~ Just (SES.content b)
      msg = SES.message (SES.content subject) bdy
      email = SES.sendEmail from dest msg
      run = (runResourceT . runAWS env $ void $ send email) :: IO ()
  liftIO run

putFile :: (HasAWS m, ToBody a) => S3.BucketName -> S3.ObjectKey -> Text -> a -> m ()
putFile bucketName key contentType file = do
  env <- getAWSEnv
  let poRe = S3.putObject bucketName key (toBody file)
      run = (runResourceT . runAWS env $
               void $ send (poRe & S3.poContentType .~ Just contentType)) :: IO ()
  liftIO run

getFile :: HasAWS m => S3.BucketName -> S3.ObjectKey -> m LByteString
getFile bucketName key = do
  env <- getAWSEnv
  let goRe = S3.getObject bucketName key
      run = (runResourceT $ do
                resp <- runAWS env $ send goRe
                (source, finalizer) <- unwrapResumable $ _streamBody (resp ^. S3.gorsBody)
                lbs <- source $$ sinkLbs
                finalizer
                return lbs) :: IO LByteString
  liftIO run
