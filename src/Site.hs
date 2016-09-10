{-# LANGUAGE ExtendedDefaultRules,
             FlexibleContexts,
             NoImplicitPrelude,
             OverloadedStrings,
             RecordWildCards,
             TemplateHaskell #-}
module Site
  ( app
  ) where

import Api
import App
import Asciify (decodeGSImage)
import ClassyPrelude
import Codec.Picture
import Control.Lens
import Domino.DoubleSix
import Domino.DoubleSix.Stats
import Image
import Lucid hiding (with)
import Servant hiding (serveDirectory, POST, addHeader)
import Snap.Core
import Snap.Util.FileServe
import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Snaplet.Auth.Backends.PostgresqlSimple
import Snap.Snaplet.PostgresqlSimple
import Snap.Snaplet.Session.Backends.CookieSession
import SnapUtil
import System.FilePath.Posix
import qualified Network.Wreq as Wreq

api :: Servant.Proxy (Api AppHandler)
api = Servant.Proxy

routes :: [(ByteString, AppHandler ())]
routes =
  [ ("/images", serveDirectory "static/images")
  , ("/", landingHandler)
  , ("/image", imageHandler)
  , ("/rq/:imageId", newRqHandler)
  ]

newRqHandler :: AppHandler ()
newRqHandler = do
  Just imgId <- getFromParam "imageId"
  lucid $ masterPage $ do
    bs <- toStrict <$> getImageBlob imgId
    case decodeGSImage bs of
      Right img -> renderStats $ getStats ( novemDoubleSix img 30 )
      Left er -> span_ $ toHtml er

apiServer :: Servant.Server (Api AppHandler) AppHandler
apiServer =
       undefined
  :<|> undefined

-- servantApp :: Application AppHandler
servantApp = serve api apiServer

app :: SnapletInit App App
app = makeSnaplet "domino website" description Nothing $ do
    d <- nestSnaplet "pg-db" db $ pgsInit' $ pgsDefaultConfig "host=localhost port=5432 dbname=domino"
    a <- nestSnaplet "auth" auth $ initPostgresAuth sess d
    s <- nestSnaplet "sess" sess $ initCookieSessionManager "key" "domino-session"
                                     Nothing (Just (86400*14))

    addRoutes routes

    return $ App s a d
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
  h2_ "hello"
  form_ [ method_ "post", action_ "image" ] $ do
    input_ [ name_ "url", type_ "text" ]
    input_ [ type_ "submit", value_ "Submit" ]

masterPage :: Monad m => HtmlT m () -> HtmlT m ()
masterPage inner =
  html_ $ do
    head_ $ do
      link_ [ href_ "https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/3.3.7/css/bootstrap.css"
            , rel_ "stylesheet"
            ]
    body_ $ do
      inner

imageHandler :: AppHandler ()
imageHandler = method POST $ do
  Just url <- getPostParam "url"
  let stringUrl = unpack $ decodeUtf8 url
  r <- liftIO $ Wreq.get stringUrl
  let bs = r ^. Wreq.responseBody
      ct = r ^. Wreq.responseHeader "Content-Type"
  mImgId <- with db $ createImage (decodeUtf8 ct) bs
  case mImgId of
    -- TODO: better error handling
    Nothing -> redirect "/"
    Just imgId -> redirect ("/rq/" ++ encodeUtf8 (tshow imgId))

renderStats :: Monad m => Stats -> HtmlT m ()
renderStats Stats{..} =
    div_ [ class_ "row" ] $ do
      div_ [ class_ "col-md-8" ] $ do
        forM_ dominoRows $ \row -> do
          div_ $ forM_ row $ \cell -> do
            doubleSixToHtml cell
      div_ [ class_ "col-md-3" ] $ do
        return ()
      div_ [ class_ "col-md-2" ] $ do
        displayCounts dominoCounts
  where
    renderPrice :: Monad m => HtmlT m ()
    renderPrice = do
      return ()

displayCounts :: Monad m => [(DoubleSix, Int)] -> HtmlT m ()
displayCounts counts = do
  span_ "total"
  span_ $ toHtml (tshow $ sum $ map snd counts)
  div_ $ do
    forM_ counts $ \(ds, amount) -> do
      doubleSixToHtml ds
      span_ $ toHtml (tshow amount)

lucid :: MonadSnap m => HtmlT m a -> m ()
lucid response = do
  modifyResponse $ addHeader "Content-Type"  "text/html; charset=UTF-8"
  writeLBS =<< renderBST response

doubleSixToHtml :: Monad m => DoubleSix -> HtmlT m ()
doubleSixToHtml (DoubleSix l r) =
    img_ [ src_ ("/images/double-six/" ++ sectionToWord l ++ "-" ++ sectionToWord r ++ ".png")
         , style_ "width: 25px;"
         ]
  where
    sectionToWord :: Section -> Text
    sectionToWord Six = "six"
    sectionToWord Five = "five"
    sectionToWord Four = "four"
    sectionToWord Three = "three"
    sectionToWord Two = "two"
    sectionToWord One = "one"
    sectionToWord Blank = "blank"


