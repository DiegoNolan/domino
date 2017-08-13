{-# LANGUAGE ExtendedDefaultRules,
             FlexibleContexts,
             NoImplicitPrelude,
             OverloadedStrings,
             RecordWildCards,
             TemplateHaskell #-}
module Site
  ( app
  ) where

import App
import Asciify (decodeGSImage, decodeHSVImage)
import ClassyPrelude
import Codec.Picture
import Control.Lens
import qualified Data.Text as Text
import DoubleSix
import Image
import Lucid hiding (with)
import Network.AWS
import Servant hiding (serveDirectory, POST, addHeader)
import Servant.Server.Internal.SnapShims
import Shared.Api
import Shared.Domino.DoubleSix
import Shared.Domino.DoubleSix.Stats
import Snap.Core
import Snap.Util.FileServe
import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Snaplet.Auth.Backends.PostgresqlSimple
import Snap.Snaplet.AWS
import Snap.Snaplet.PostgresqlSimple
import Snap.Snaplet.Session.Backends.CookieSession
import SnapUtil
import System.FilePath.Posix
import qualified Network.Wreq as Wreq

api :: Servant.Proxy Api
api = Servant.Proxy

routes :: [(ByteString, AppHandler ())]
routes =
  [ ("/js", serveDirectory "static/ghcjs")
  , ("/images", serveDirectory "static/images")
  , ("/", landingHandler)
  ]

apiServer :: Servant.Server Api AppHandler
apiServer =
       newImageHandler
  :<|> getRowsHandler

-- servantApp :: Application AppHandler
servantApp = serve api apiServer

app :: SnapletInit App App
app = makeSnaplet "domino website" description Nothing $ do
    d <- nestSnaplet "pg-db" db $ pgsInit' $ pgsDefaultConfig "host=localhost port=5432 dbname=domino"
    a <- nestSnaplet "auth" auth $ initPostgresAuth sess d
    s <- nestSnaplet "sess" sess $ initCookieSessionManager "key" "domino-session"
                                     Nothing (Just (86400*14))
    env <- liftIO $ newEnv Oregon Discover
    am <- nestSnaplet "aws" aws $ awsInit env

    addRoutes $ routes ++ [("api", applicationToSnap servantApp)]

    return $ App s a d am
  where
    description = "A website for setting domino art"

handleLogin :: Maybe Text -> Handler App (AuthManager App) ()
handleLogin authError = undefined

{-
setCache :: MonadSnap m => m a -> m ()
setCache action = do
    pinfo <- liftM rqPathInfo getRequest
    action
    when ("media" `B.isPrefixOf` pinfo) $ do
       expTime <- liftM (+604800) $ liftIO epochTime
       s       <- liftIO $ formatHttpTime expTime
       modifyResponse $
          setHeader "Cache-Control" "public, max-age=604800" .
          setHeader "Expires" s
-}

landingHandler :: AppHandler ()
landingHandler = lucid $ masterPage $ do
  div_ [ id_ "create" ] ""
  script_ [ src_ "/js/create.js" ] ""

masterPage :: Monad m => HtmlT m () -> HtmlT m ()
masterPage inner =
  html_ $ do
    head_ $ do
      link_ [ href_ "https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/3.3.7/css/bootstrap.css"
            , rel_ "stylesheet"
            ]
    body_ $ do
      inner

newImageHandler :: Maybe Text -> AppHandler Int
newImageHandler mUrl = do
  print mUrl
  Just url <- return mUrl
  let stringUrl = unpack (Text.strip url)
  r <- liftIO $ Wreq.get stringUrl
  let bs = r ^. Wreq.responseBody
      ct = r ^. Wreq.responseHeader "Content-Type"
  mImgId <- createImage (decodeUtf8 ct) bs
  case mImgId of
    Nothing -> do logError "Error creating image"
                  finishWith (emptyResponse & setResponseCode 500)
    Just imgId -> return $ unImageId imgId

getRowsHandler :: Maybe Int -> Maybe Double -> AppHandler [[DoubleSix]]
getRowsHandler mi mw = do
  Just i <- return mi
  Just w <- return mw
  putStr $ "Width : " ++ tshow w
  let imgId = ImageId i
  bs <- toStrict <$> getImageBlob imgId
  case decodeGSImage bs of
    Right img -> do
      let rows = scaleDoubleSix img (desiredWidthToCount (w * 12))
      return rows
    Left er -> do logError $ encodeUtf8 $ pack ("Error decoding image : " ++ er)
                  finishWith (emptyResponse & setResponseCode 500)

lucid :: MonadSnap m => HtmlT m a -> m ()
lucid response = do
  modifyResponse $ addHeader "Content-Type"  "text/html; charset=UTF-8"
  writeLBS =<< renderBST response

