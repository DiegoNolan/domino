{-# LANGUAGE ExtendedDefaultRules,
             FlexibleContexts,
             FlexibleInstances,
             NoImplicitPrelude,
             OverloadedStrings,
             RankNTypes,
             RecordWildCards,
             TemplateHaskell,
             TypeOperators #-}
module Site
  ( runApp
  ) where

import ClassyPrelude hiding (Handler)
import Asciify (decodeGSImage, decodeHSVImage)
import AWS
import Codec.Picture
import Control.Lens
import Control.Monad.Error.Class
import qualified Data.Text as Text
import Database.PostgreSQL.Simple
import DoubleSix
import Image
import Lucid hiding (with)
import Network.AWS
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Shared.Api
import Shared.Domino.DoubleSix
import Shared.Domino.DoubleSix.Stats
import System.FilePath.Posix
import qualified Network.Wreq as Wreq

runApp :: IO ()
runApp = do
  cfg <- getConfig
  run 8000 (app cfg)

wholeApi :: Proxy WholeApi
wholeApi = Proxy

api :: Proxy Api
api = Proxy

wrapperApi :: Proxy WrapperApi
wrapperApi = Proxy

staticApi :: Proxy StaticApi
staticApi = Proxy

getConfig :: IO Config
getConfig = do
  conn <- connect $ ConnectInfo "localhost" 5432 "root" "" "domino"
  env <- newEnv Discover
  return $ Config { confEnv = (env & envRegion .~ Oregon)
                  , confConn = conn
                  }

app :: Config -> Application
app cfg = serve wholeApi (server cfg)

server :: Config -> Server WholeApi
server cfg =
       staticServer
  :<|> enter (readerToHandler cfg) serverT

staticServer :: Server StaticApi
staticServer =
       serveDirectoryFileServer "static/ghcjs"
  :<|> serveDirectoryFileServer "static/images"

data Config = Config
  { confEnv :: Env
  , confConn :: Connection -- TODO: Pool later
  }

instance MonadIO m => HasPostgres (ReaderT Config m) where
  getPostgresState = confConn <$> ask

instance MonadIO m => HasAWS (ReaderT Config m) where
  getAWSEnv = confEnv <$> ask

readerToHandler' :: forall a. Config -> ReaderT Config IO a -> Handler a
readerToHandler' cfg m = liftIO $ runReaderT m cfg

readerToHandler :: Config -> ReaderT Config IO :~> Handler
readerToHandler cfg = NT (readerToHandler' cfg)

serverT :: (HasPostgres m, HasAWS m)
        => ServerT WrapperApi m
serverT =
       return IndexPage
  :<|>
  (    newImageHandler
  :<|> getRowsHandler
  )

newImageHandler :: (HasPostgres m, HasAWS m) => Maybe Text -> m Int
newImageHandler mUrl = do
  print mUrl
  Just url <- return mUrl
  let stringUrl = unpack (Text.strip url)
  r <- liftIO $ Wreq.get stringUrl
  let bs = r ^. Wreq.responseBody
      ct = r ^. Wreq.responseHeader "Content-Type"
  mImgId <- createImage (decodeUtf8 ct) bs
  case mImgId of
    Nothing -> error "unable"
    -- throwError $ err500 { errBody = "Unable to download image" }
    Just imgId -> return $ unImageId imgId

getRowsHandler :: (HasPostgres m, HasAWS m)
               => Maybe Int -> Maybe Double -> m [[DoubleSix]]
getRowsHandler mi mw = do
  Just i <- return mi
  Just w <- return mw
  putStr $ "Width : " ++ tshow w
  let imgId = ImageId i
  bs <- toStrict <$> getImageBlob imgId
  case decodeGSImage bs of
    Right img -> do
      let rows = scaleDoubleSix
                 img
                 (desiredWidthToCount (w * 12))
      return rows
    Left er -> error "error"
    --throwError $ err500 { errBody = encodeUtf8 ( pack ("Error decoding image : " ++ er)) }

