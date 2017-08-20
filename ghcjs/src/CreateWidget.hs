{-# LANGUAGE FlexibleContexts,
             OverloadedStrings,
             DataKinds,
             LambdaCase,
             PolyKinds,
             RecursiveDo,
             RecordWildCards,
             ScopedTypeVariables,
             TypeOperators,
             RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
module CreateWidget
  ( app
  ) where

import ClientAPI
import Control.Monad
import GHCJS.DOM
import GHCJS.DOM.Element (getClientWidth)
import GHCJS.DOM.Node
import GHCJS.DOM.Document hiding (select)
import GHCJS.DOM.Types hiding (Event, Text)
import Data.Monoid
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Map as Map
import JavaScript.Web.Location
import Reflex
import Reflex.Dom
import Reflex.Host.Class
import Safe
import Servant.API
import Servant.Reflex
import Shared.Domino.DoubleSix
import Shared.Domino.DoubleSix.Stats
import Shared.Image

app :: IO ()
app = mainWidgetInElementById "create" createWidget

data CreationFlow =
    NoUrl
  | NewUrl Text
  | NewImage Int ArtOptions
  | DisplayStats Int [[DoubleSix]] ArtOptions
  | FetchFailure Text

createWidget :: forall t m. MonadWidget t m => m ()
createWidget = mdo
  dynEventStream <- widgetHold newUrlWidget (creationFlowWidgetMapper <$> widgetEvents)
  let widgetEvents = switchPromptlyDyn dynEventStream
  return ()

creationFlowWidgetMapper :: MonadWidget t m => CreationFlow -> m (Event t CreationFlow)
creationFlowWidgetMapper NoUrl = newUrlWidget
creationFlowWidgetMapper (NewUrl url) = fetchImageWidget url
creationFlowWidgetMapper (NewImage i w) = fetchStatsWidget i w
creationFlowWidgetMapper (DisplayStats i stats ao) = dominoArtWidget i stats ao
creationFlowWidgetMapper (FetchFailure s) = do
  text ("Fetch failure : " <> s)
  return never

newUrlWidget :: MonadWidget t m => m (Event t CreationFlow)
newUrlWidget = do
  urlDyn <- inputOnEnter
  return $ NewUrl <$> updated urlDyn

type Test = "test" :> Get '[JSON] Int

fetchImageWidget :: MonadWidget t m => Text -> m (Event t CreationFlow)
fetchImageWidget url = do
  -- result <- test
  postBuild <- getPostBuild
  reqResult <- newImage (constDyn (QParamSome url)) postBuild
  text "Fetching Image"
  return $ (\case
               ResponseSuccess _ i _ -> NewImage i defaultArtOptions
               ResponseFailure _ e _ -> FetchFailure e
               RequestFailure _ e    -> FetchFailure e
           ) <$> reqResult

fetchStatsWidget :: MonadWidget t m => Int -> ArtOptions -> m (Event t CreationFlow)
fetchStatsWidget i (ao@ArtOptions{..}) = do
  postBuild <- getPostBuild
  reqResult <- getArtRows (constDyn (QParamSome i))
                          (constDyn (QParamSome targetWidth)) postBuild
  text "Processing Results"
  return $ (\case
               ResponseSuccess _ rs _ -> DisplayStats i rs ao
               ResponseFailure _ e _  -> FetchFailure e
               RequestFailure _ e     -> FetchFailure e
           ) <$> reqResult

dominoArtWidget :: MonadWidget t m => Int -> [[DoubleSix]]
                -> ArtOptions -> m (Event t CreationFlow)
dominoArtWidget imgId rows initArtOptions = mdo
  artOptions <- elClass "div" "row" $ mdo
    elClass "div" "col-xs-9" $ do
      renderDominos rows
      elAttr "img" (Map.fromList
                    [ ("src", Text.pack (imageUrl imgId))
                    , ("style", "width: 100%")
                    ]
                   ) (return ())
    elClass "div" "col-xs-3" $ do
      ao <- artOptionsWidget initArtOptions
      renderStatsBar (getStats rows)
      return ao
  return $ NewImage imgId <$> updated artOptions

data ArtOptions = ArtOptions
  { targetWidth :: Double
  }

defaultArtOptions :: ArtOptions
defaultArtOptions =
  ArtOptions
    { targetWidth = 6
    }

artOptionsWidget :: MonadWidget t m => ArtOptions -> m (Dynamic t ArtOptions)
artOptionsWidget ArtOptions{..} = do
  (textValue, submitEv) <- elClass "div" "col-sm-6 col-xs-12" $ do
    elClass "div" "input-group" $ do
      tValue <- textInput $ def
                  & textInputConfig_attributes .~ constDyn ("class" =: "form-control")
                  & textInputConfig_initialValue .~  Text.pack (show targetWidth)
      (submitEl, _) <- elClass "div" "input-group-btn" $ do
        elAttr' "button" ("class" =: "btn btn-default") (text "Update")
      return $ (tValue, domEvent Click submitEl)
  let enterEvent = ffilter (== 13) (_textInput_keyup textValue)
      valueEvent = tagDyn (_textInput_value textValue)
                       (leftmost [ tag (constant ()) enterEvent, submitEv ])
  holdDyn defaultArtOptions (ArtOptions <$> fmapMaybe (readMay . Text.unpack) valueEvent)

renderStatsBar :: MonadWidget t m => Stats -> m ()
renderStatsBar Stats{..} = do
    elClass "div" "row" $ do
      elClass "div" "col-xs-12 center-text" $ el "h3" (text "Info")

    elClass "div" "row" $ do
      elClass "div" "col-xs-12" $ do
        let (w,h) = physicalDimensions
        text $ feetToReadable w <> " by " <> feetToReadable h

    elClass "div" "row" $ do
      elClass "div" "col-xs-12" $ text (Text.pack (show totalDominoes) <>
                                        " Total Dominoes")

    elClass "div" "row" $ do
      elClass "div" "col-xs-12" $ renderCounts dominoCounts

    elClass "div" "row" $ do
      elClass "div" "col-xs-12" $ do
        text $ "Estimated weight : " <> Text.pack (show weight)

    elClass "div" "row" $ do
      elClass "div" "col-xs-12" $ do
        text $ "Price : $" <> Text.pack (show ((price + 50) `div` 100))
  where
    feetToReadable :: Double -> Text
    feetToReadable d = Text.pack $ show f ++ "' " ++ show inches ++ "\""
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
    cnt = fromIntegral $ length (head rs) :: Double
    renderRows :: MonadWidget t m => [DoubleSix] -> m ()
    renderRows r = forM_ r (renderDoubleSix (Percentage (100 / cnt)))

data DominoSize =
    Percentage Double
  | Fixed Double

renderDoubleSix :: MonadWidget t m => DominoSize -> DoubleSix -> m ()
renderDoubleSix ds (DoubleSix l r) =
    elAttr "img" (Map.fromList
                  [ ("src", "/images/double-six/" <> sectionToWord l <> "-" <>
                      sectionToWord r <> ".png")
                  , ("style", "width: " <> width)
                  ]
                 ) (return ())
  where
    width = case ds of
              Percentage d -> Text.pack $ show d ++ "%"
              Fixed d -> Text.pack $ show d ++ "px"
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
          el "td" (text . Text.pack $ show amount)
          el "td" $ renderDoubleSix (Fixed 70) ds

inputOnEnter :: MonadWidget t m => m (Dynamic t Text)
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

