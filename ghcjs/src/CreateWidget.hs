{-# LANGUAGE OverloadedStrings,
             NoImplicitPrelude,
             LambdaCase,
             RecursiveDo,
             RecordWildCards,
             ScopedTypeVariables,
             RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
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

newImageWidget :: MonadWidget t m => Int -> m ()
newImageWidget i = do
  let ( newImage :<|> getStats ) = client (Proxy :: Proxy Api)
                                          (Proxy :: MonadWidget t m => Proxy m)
                                          (constDyn (BasePath "/api"))
  buildEv <- getPostBuild
  result <- getStats (constant (Right i)) (tag (constant ()) buildEv)
  resultDyn <- holdDyn Nothing (Just <$> result)
  artOptionsRenderEvent <- dyn =<< handleResult (dominoArtWidget i) `mapDyn` resultDyn
  let a = artOptionsRenderEvent :: Event _ (Maybe (Dynamic _ ArtOptions))
  return ()

dominoArtWidget :: MonadWidget t m => Int -> Stats -> m (Dynamic t ArtOptions)
dominoArtWidget imgId stats = mdo
  elClass "div" "row" $ mdo
    elClass "div" "col-xs-9" $ do
      renderDominos (dominoRows stats)
      elAttr "img" (Map.fromList
                    [ ("src", imageUrl imgId)
                    , ("style", "width: 100%")
                    ]
                   ) (return ())
    elClass "div" "col-xs-3" $ do
      artOptions <- artOptionsWidget
      renderStasBar stats
      return artOptions

data ArtOptions = ArtOptions
  { targetWidth :: Double
  }

defaultArtOptions :: ArtOptions
defaultArtOptions =
  ArtOptions
    { targetWidth = 3
    }

artOptionsWidget :: MonadWidget t m => m (Dynamic t ArtOptions)
artOptionsWidget = do
  (textValue, submitEv) <- elClass "div" "col-sm-6 col-xs-12" $ do
    elClass "div" "input-group" $ do
      tValue <- textInput $ def
                  & textInputConfig_attributes .~ constDyn ("class" =: "form-control")
                  & textInputConfig_initialValue .~ "3"
      (submitEl, _) <- elClass "div" "input-group-btn" $ do
        elAttr' "button" ("class" =: "btn btn-default") (text "Update")
      return $ (tValue, domEvent Click submitEl)
  let enterEvent = ffilter (== 13) (_textInput_keyup textValue)
      valueEvent = tagDyn (_textInput_value textValue)
                       (leftmost [ tag (constant ()) enterEvent, submitEv ])
  holdDyn defaultArtOptions (ArtOptions <$> fmapMaybe readMay valueEvent)

renderStasBar :: MonadWidget t m => Stats -> m ()
renderStasBar Stats{..} = do
    elClass "div" "row" $ do
      elClass "div" "col-xs-12 center-text" $ el "h3" (text "Info")

    elClass "div" "row" $ do
      elClass "div" "col-xs-12" $ do
        let (w,h) = physicalDimensions
        text $ feetToReadable w ++ " by " ++ feetToReadable h

    elClass "div" "row" $ do
      elClass "div" "col-xs-12" $ text (show totalDominoes ++ " Total Dominoes")

    elClass "div" "row" $ do
      elClass "div" "col-xs-12" $ renderCounts dominoCounts
  where
    feetToReadable :: Double -> String
    feetToReadable d = show f ++ "' " ++ show inches ++ "\""
      where
        sixInFoot = 12 * 16
        sixteenths = round (d * fromIntegral sixInFoot) :: Int
        (f, sixs) = divMod sixteenths sixInFoot
        (inches, _) = divMod sixs 16


displayInt :: MonadWidget t m => Dynamic t Int -> m ()
displayInt = display

renderDominos :: MonadWidget t m => [[DoubleSix]] -> m ()
renderDominos rs = forM_ rs $ \row -> el "div" (renderRows row)
  where
    cnt = fromIntegral $ length (headEx rs) :: Double
    renderRows :: MonadWidget t m => [DoubleSix] -> m ()
    renderRows r = forM_ r (renderDoubleSix (Percentage (100 / cnt)))

data DominoSize =
    Percentage Double
  | Fixed Double

renderDoubleSix :: MonadWidget t m => DominoSize -> DoubleSix -> m ()
renderDoubleSix ds (DoubleSix l r) =
    elAttr "img" (Map.fromList
                  [ ("src", "/images/double-six/" ++ unpack (sectionToWord l) ++ "-" ++
                      unpack (sectionToWord r) ++ ".png")
                  , ("style", "width: " ++ width)
                  ]
                 ) (return ())
  where
    width = case ds of
              Percentage d -> show d ++ "%"
              Fixed d -> show d ++ "px"
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
  el "table" $ do
    el "tbody" $ do
      forM_ counts $ \(ds, amount) ->
        el "tr" $ do
          el "td" (text $ show amount)
          el "td" $ renderDoubleSix (Fixed 70) ds

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

