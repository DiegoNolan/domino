{-# LANGUAGE LambdaCase,
             MultiWayIf,
             NoImplicitPrelude,
             OverloadedStrings,
             TemplateHaskell #-}
module Image.Generation
  ( generateDoubleSixLayout
  , generateFromGrid
  , generateFromMaybeGrid
  ) where

import ClassyPrelude
import qualified Asciify as A
import Codec.Picture
import Data.List ((!!))
import Shared.Domino.DoubleSix

generateFromGrid :: Pixel a => [[a]] -> Image a
generateFromGrid rows =
  generateImage (\i j -> (rows !! j) !! i)
  (length $ headEx rows) (length rows)

generateFromMaybeGrid :: [[Maybe PixelRGB8]] -> Image PixelRGB8
generateFromMaybeGrid rows =
  generateImage
  (\i j -> case (rows !! j) !! i of
             Nothing -> PixelRGB8 255 255 255
             Just p -> p
  ) (length $ headEx rows) (length rows)

{-
generatePuzzle :: Pixal a => Puzzle a -> Image a
generatePuzzle puzz =
-}

generateDoubleSixLayout :: [[DoubleSix]] -> Image Pixel8
generateDoubleSixLayout rows =
    generateImage genFunc
    (domWidth * pixelsPerWidth) (domHeight * pixelsPerHeight)
  where
    pixelsPerWidth = 100
    pixelsPerHeight = 50
    borderSize = 2
    radius = 5
    domWidth = length (rows !! 0)
    domHeight = length rows
    genFunc i j = drawDomino currDomino xr yr
      where
        (x, xr) = i `divMod` pixelsPerWidth
        (y, yr) = j `divMod` pixelsPerHeight
        currDomino = (rows !! y) !! x
    drawDomino :: DoubleSix -> Int -> Int -> Word8
    drawDomino dom i j
        | r == 0    = drawSide (left dom) ni j
        | otherwise = drawSide (right dom) ni j
      where
        (r, ni) = i `divMod` (pixelsPerWidth `div` 2)

    drawSide :: Section -> Int -> Int -> Word8
    drawSide side i j
        | inBorder  = 0
        | otherwise =
          case side of
            Blank -> 255
            One   -> if inCenterCircle then 0 else 255
            Two   -> if inTopLeft || inBotRight then 0 else 255
            Three -> if inTopLeft || inCenterCircle || inBotRight then 0 else 255
            Four  -> if inTopLeft || inTopRight || inBotLeft || inBotRight
                     then 0
                     else 255
            Five  -> if inTopLeft || inTopRight || inBotLeft ||
                        inBotRight || inCenterCircle
                     then 0
                     else 255
            Six   -> if inTopLeft || inTopCenter || inTopRight ||
                        inBotLeft || inBotCenter || inBotRight
                     then 0
                     else 255
      where
        pixW = fromIntegral pixelsPerWidth
        pixH = fromIntegral pixelsPerHeight
        inTopLeft =
          (fromIntegral i - pixW / 8) ^^ 2 +
          (fromIntegral j - pixH / 4) ^^ 2 <=
          (fromIntegral radius) ^^ 2
        inTopRight =
          (fromIntegral i - 3 * pixW / 8) ^^ 2 +
          (fromIntegral j - pixH / 4) ^^ 2 <=
          (fromIntegral radius) ^^ 2
        inBotLeft =
          (fromIntegral i - pixW / 8) ^^ 2 +
          (fromIntegral j - 3 * pixH / 4) ^^ 2 <=
          (fromIntegral radius) ^^ 2
        inBotRight =
          (fromIntegral i - 3 * pixW / 8) ^^ 2 +
          (fromIntegral j - 3 * pixH / 4) ^^ 2 <=
          (fromIntegral radius) ^^ 2
        inTopCenter =
          (fromIntegral i - pixW / 4) ^^ 2 +
          (fromIntegral j - pixH / 4) ^^ 2 <=
          (fromIntegral radius) ^^ 2
        inBotCenter =
          (fromIntegral i - pixW / 4) ^^ 2 +
          (fromIntegral j - 3 * pixH / 4) ^^ 2 <=
          (fromIntegral radius) ^^ 2
        inBorder =
          i <= borderSize || j <= borderSize ||
          pixelsPerWidth `div` 2 - i <= borderSize ||
          pixelsPerHeight - j <= borderSize
        inCenterCircle =
          (fromIntegral i - pixW / 4) ^^ 2 +
          (fromIntegral j - pixH / 2) ^^ 2 <=
          (fromIntegral radius) ^^ 2
