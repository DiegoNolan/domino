{-# LANGUAGE ExtendedDefaultRules,
             NoImplicitPrelude,
             OverloadedStrings,
             FlexibleContexts,
             TemplateHaskell #-}
module Site
  ( app
  ) where

import App
import ClassyPrelude
import Control.Lens
import Domino.DoubleSix
import Lucid
import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server
import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Snaplet.Auth.Backends.PostgresqlSimple
import Snap.Snaplet.PostgresqlSimple
import Snap.Snaplet.Session
import Snap.Snaplet.Session.Backends.CookieSession
import System.FilePath.Posix
import qualified Network.Wreq as Wreq

routes :: [(ByteString, AppHandler ())]
routes =
  [ ("/images", serveDirectory "static/images")
  , ("/", landingHandler)
  , ("/image", imageHandler)
  ]

app :: SnapletInit App App
app = makeSnaplet "domino website" description Nothing $ do
    d <- nestSnaplet "" db $ pgsInit' $ pgsDefaultConfig "host=localhost port=5432 dbname=domino"
    a <- nestSnaplet "" auth $ initPostgresAuth sess d
    s <- nestSnaplet "" sess $ initCookieSessionManager "key" "domino-session" Nothing (Just (86400*14))

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
landingHandler =  method GET get
  where
    get = lucid $
      html_ $ do
        head_ $ do
          return ()
        body_ $ do
          h2_ "hello"
          form_ [ method_ "post", action_ "image" ] $ do
            input_ [ name_ "url", type_ "text" ]
            input_ [ type_ "submit", value_ "Submit" ]

imageHandler :: AppHandler ()
imageHandler =
  lucid $
      html_ $ do
        head_ $ do
          link_ [ rel_ "https://cdnjs.cloudflare.com/ajax/libs/react-bootstrap/0.30.3/react-bootstrap.js"]
        body_ $ do
          mUrl <- lift $ getPostParam "url"
          case mUrl of
            Nothing -> span_ "Could not find"
            Just url -> do
              let stringUrl = unpack $ decodeUtf8 url
                  fname = "tmp/" ++ takeFileName stringUrl
              eImg <- liftIO $ do
                r <- Wreq.get stringUrl
                -- TODO: delete file
                writeFile fname (r ^. Wreq.responseBody)
                loadGSImage fname
              case eImg of
                Right img -> do
                  let rows = scaleDoubleSix img 25
                      counts = getCounts $ concat rows
                  div_ [ class_ "row" ] $ do
                    div_ [ class_ "col-md-10" ] $ do
                      forM_ rows $ \row -> do
                        div_ $ forM_ row $ \cell -> do
                          doubleSixToHtml cell
                    div_ [ class_ "col-md-2" ] $ do
                      displayCounts counts
                Left er -> span_ $ toHtml er

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
         , style_ "width: 30px;"
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

getCounts :: [DoubleSix] -> [(DoubleSix, Int)]
getCounts doms = map (\lst@(x:_) -> (x, length lst)) grouped
  where
    order (DoubleSix a b) = if a < b then DoubleSix a b else DoubleSix b a
    grouped = group $ sort (map order doms)
