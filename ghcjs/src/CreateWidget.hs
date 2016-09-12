{-# LANGUAGE OverloadedStrings,
             NoImplicitPrelude,
             LambdaCase,
             RecursiveDo,
             RankNTypes #-}
module CreateWidget
  ( app
  ) where

import ClassyPrelude
import GHCJS.DOM
import GHCJS.DOM.Element (getClientWidth)
import GHCJS.DOM.Node
import GHCJS.DOM.Document hiding (select)
import GHCJS.DOM.Types (castToHTMLElement)
import Data.Proxy
import qualified Data.Map as Map
import JavaScript.Web.Location
import Reflex
import Reflex.Dom
import Reflex.Dom.Widget.Resize
import Reflex.Host.Class
import Servant.API
import Servant.Reflex
import Shared.Api
import Shared.Domino.DoubleSix
import Shared.Domino.DoubleSix.Stats
import Shared.Image

runWidgetsOnElemsById :: [ (String
                           , Widget Spider
                               (Gui Spider (WithWebView SpiderHost) (HostFrame Spider)) ())]
                      -> IO ()
runWidgetsOnElemsById widgets = runWebGUI $ \webView -> do
  Just doc <- webViewGetDomDocument webView
  forM_ widgets $ \(elemId, widget) -> do
    Just elm <- getElementById doc elemId
    attachWidget (castToHTMLElement elm) webView widget

app :: IO ()
app = runWidgetsOnElemsById
  [ ("create", createWidget) ]

handleResult :: MonadWidget t m => (a -> m b) -> Maybe (ReqResult a) -> m (Maybe b)
handleResult f (Just (ResponseSuccess a _)) = Just <$> f a
handleResult _ (Just (ResponseFailure e _)) = do
  text e
  return Nothing
handleResult _ (Just (RequestFailure e)) = do
  text e
  return Nothing
handleResult _ _ = return Nothing

statsWidget :: MonadWidget t m => Int -> Stats -> m ()
statsWidget imgId stats = mdo
  elClass "div" "row" $ mdo
    (e,(resizeEvent,_)) <- elAttr' "div" ("class" =: "col-xs-10") $ resizeDetector $ do
      renderDominos width (dominoRows stats)
      elAttr "img" (Map.fromList
                    [ ("src", imageUrl imgId)
                    , ("style", "width: 100%")
                    ]
                   ) (return ())
    elClass "div" "col-xs-2" $ do
      renderCounts (dominoCounts stats)
    buildEvent <- getPostBuild
    widthEv <- performEvent $ const (getClientWidth (_el_element e)) <$>
               leftmost [ resizeEvent, buildEvent ]
    width <- foldDyn (\v _ -> v - 40) 700 widthEv
    return ()


newImageWidget :: MonadWidget t m => Int -> m ()
newImageWidget i = do
  let ( newImage :<|> getStats ) = client (Proxy :: Proxy Api)
                                          (Proxy :: MonadWidget t m => Proxy m)
                                          (constDyn (BasePath "/api"))
  buildEv <- getPostBuild
  result <- getStats (constant (Right i)) (tag (constant ()) buildEv)
  resultDyn <- holdDyn Nothing (Just <$> result)
  dyn =<< handleResult (statsWidget i) `mapDyn` resultDyn
  return ()

createWidget :: forall t m. MonadWidget t m => m ()
createWidget = do
  let ( newImage :<|> getStats ) = client (Proxy :: Proxy Api)
                                          (Proxy :: MonadWidget t m => Proxy m)
                                          (constDyn (BasePath "/api"))


  urlDyn <- inputOnEnter

  -- TODO: Fix this. It is stupid
  createImageEv <- delay 0.1 (tag (constant ()) (updated urlDyn))
  newImageRq <- newImage (Right . pack <$> current urlDyn) createImageEv
  newImageRqDyn <- holdDyn Nothing (Just <$> newImageRq)
  dyn =<< handleResult newImageWidget `mapDyn` newImageRqDyn

  return ()

displayInt :: MonadWidget t m => Dynamic t Int -> m ()
displayInt = display

renderDominos :: MonadWidget t m => Dynamic t Double -> [[DoubleSix]] -> m ()
renderDominos fullWidth rows = do
    dynDominoWidth <- mapDyn (\w -> w / count) fullWidth
    forM_ rows $ \row -> el "div" (renderRows dynDominoWidth row)
  where
    count = fromIntegral $ length (headEx rows)
    renderRows :: MonadWidget t m => Dynamic t Double -> [DoubleSix] -> m ()
    renderRows w row = forM_ row (renderDoubleSix w)

renderDoubleSix :: MonadWidget t m => String -> DoubleSix -> m ()
renderDoubleSix (DoubleSix l r) =
    elAttr "img" (Map.fromList
                  [ ("src", "/images/double-six/" ++ unpack (sectionToWord l) ++ "-" ++
                      unpack (sectionToWord r) ++ ".png")
                  , ("style", "width: 3.33333%")
                  ]
                 ) (return ())
  where
    sectionToWord :: Section -> Text
    sectionToWord Six = "six"
    sectionToWord Five = "five"
    sectionToWord Four = "four"
    sectionToWord Three = "three"
    sectionToWord Two = "two"
    sectionToWord One = "one"
    sectionToWord Blank = "blank"

renderCounts :: MonadWidget t m => [(DoubleSix, Int)] -> m ()
renderCounts counts = do
  forM_ counts $ \(ds, amount) -> do
    renderDoubleSix (constDyn 40) ds
    el "span" (text $ show amount)

inputOnEnter :: MonadWidget t m => m (Dynamic t String)
inputOnEnter =
  elClass "div" "row" $ do
    elClass "div" "col-sm-3 col-xs-0" (return ())
    (textValue, submitEv) <- elClass "div" "col-sm-6 col-xs-12" $ do
      elClass "div" "input-group" $ do
        tValue <- textInput $ def
                    & textInputConfig_attributes .~ constDyn
                          (Map.fromList
                            [ ("class", "form-control")
                            , ("placeholder", "Image URL")
                            ])
        (submitEl, _) <- elClass "div" "input-group-btn" $ do
          elAttr' "button" (Map.fromList [ ("class", "btn btn-default")]
                            ) (text "Submit")
        return $ (tValue, domEvent Click submitEl)
    elClass "div" "col-sm-2 col-xs-0" (return ())
    let enterEvent = ffilter (== 13) (_textInput_keyup textValue)
    holdDyn "" (tagDyn (_textInput_value textValue)
                    (leftmost [ enterEvent, tag (constant 0) submitEv ]))

