{-# LANGUAGE NoImplicitPrelude,
             LambdaCase,
             OverloadedStrings,
             TemplateHaskell #-}
module DoubleSix
  ( scaleDoubleSix
  , novemDoubleSix
  , invertGrayscale
  , sobel
  , amplifyEdges
  , gaussianFilter
  , generateDoubleSixLayout
  ) where

import ClassyPrelude
import Asciify hiding (Blank)
import Data.List ((!!))
import Debug.Trace
import qualified Asciify as A
import Codec.Picture
import Codec.Picture.Types
import Shared.Domino.DoubleSix

scaleDoubleSix :: Image Pixel8 -> Int -> [[DoubleSix]]
scaleDoubleSix img chsWide = quadAsciify' img chsWide quadMapping
  --scaleAsciify' img chsWide scaleMapping

generateDoubleSixLayout :: [[DoubleSix]] -> Image Pixel8
generateDoubleSixLayout rows =
    generateImage genFunc
    (domWidth * pixelsPerWidth) (domHeight * pixelsPerHeight)
  where
    pixelsPerWidth = 100
    pixelsPerHeight = 50
    borderSize = 3
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

hij :: Float -> Int -> Int -> Int -> Float
hij sigma k i j = (1 / (2 * pi * var)) * exp inner
  where
    num = (fromIntegral i - fromIntegral k - 1) ^^ 2 +
          (fromIntegral j - fromIntegral k - 1) ^^ 2
    inner = negate $ num / (2 * var)
    var = sigma ^^ 2

invertGrayscale :: Image Pixel8 -> Image Pixel8
invertGrayscale = pixelMap (\p -> maxBound - p)

gaussianFilter :: Image Pixel8 -> Image Pixel8
gaussianFilter = pixelMap round . convolution gaussianKernal

gaussianKernal :: Kernal
gaussianKernal = Kernal
    (f 1 1, f 1 2, f 1 3)
    (f 2 1, f 2 2, f 2 3)
    (f 3 1, f 3 2, f 3 3)
  where
    f = hij 1.4 1

sobel :: Image Pixel8 -> Image Pixel8
sobel image = clampFloatImage $ generateImage
              (\i j -> sqrt
                      ( (pixelAt gx i j) ** 2 + (pixelAt gy i j) ** 2 )
              ) (imageWidth image) (imageHeight image)
  where
    gx = sobelX image
    gy = sobelY image

clampFloatImage :: Image PixelF -> Image Pixel8
clampFloatImage img = pixelMap f img
  where
    (mx, mn) = pixelFold (\(x, n) _ _ pix -> (max x pix, min n pix))
                         (-10000, 10000) img
    mxd = mx
    mnd = mn
    f p = round $ (255 / (mxd - mnd)) * p - (255 * mnd) * (mxd - mnd)

amplifyEdges :: Image Pixel8 -> Image Pixel8
amplifyEdges img =
    clampFloatImage $ generateImage
    (\i j -> fromIntegral (pixelAt img i j) + sobelAmg * fromIntegral (pixelAt s i j))
    (imageWidth img) (imageHeight img)
  where
    sobelAmg = 2
    s = sobel img

sobelX :: Image Pixel8 -> Image PixelF
sobelX = convolution kernal
  where
    kernal = Kernal
      (1, 0, -1)
      (2, 0, -2)
      (1, 0, -1)

sobelY :: Image Pixel8 -> Image PixelF
sobelY = convolution kernal
  where
    kernal = Kernal
      (1, 2, 1)
      (0, 0, 0)
      (-1, -2, -1)

testAlg :: String -> String -> (Image Pixel8 -> Image Pixel8) -> IO ()
testAlg inFile outFile f = do
  bs <- readFile inFile
  case decodeGSImage bs of
    Left _ -> putStr "Failed"
    Right i -> writePng outFile (f i)

data Kernal = Kernal (Float, Float, Float) (Float, Float, Float) (Float, Float, Float)
  deriving (Show)

convolution :: Kernal -> Image Pixel8 -> Image PixelF
convolution (Kernal (tl,tm,tr) (ml,mm,mr) (bl,bm,br)) image =
    generateImage genFunc w h
  where
    w = imageWidth image
    h = imageHeight image
    genFunc i j = (if l >= 0 && t >= 0 then pixAt l t * br else 0) +
                  (if           t >= 0 then pixAt i t * bm else 0) +
                  (if r <  w && t >= 0 then pixAt r t * bl else 0) +
                  (if l >= 0           then pixAt l j * mr else 0) +
                                            pixAt i j * mm         +
                  (if r <  w           then pixAt r j * ml else 0) +
                  (if l >= 0 && b <  h then pixAt l b * tr else 0) +
                  (if           b <  h then pixAt i b * tm else 0) +
                  (if r <  w && b <  h then pixAt r b * tl else 0)
      where
        l = i - 1
        r = i + 1
        t = j - 1
        b = j + 1
    pixAt x y = fromIntegral $ pixelAt image x y

scaleMapping :: Word8 -> DoubleSix
scaleMapping n
  | n >= 240      = DoubleSix Six Six
  | n >= 220      = DoubleSix Six Five
  | n >= 200      = DoubleSix Five Five
  | n >= 180      = DoubleSix Five Four
  | n >= 160      = DoubleSix Four Four
  | n >= 140      = DoubleSix Four Three
  | n >= 120      = DoubleSix Three Three
  | n >= 100      = DoubleSix Three Two
  | n >= 80       = DoubleSix Two Two
  | n >= 60       = DoubleSix Two One
  | n >= 40       = DoubleSix One One
  | n >= 20       = DoubleSix One Blank
  | otherwise     = DoubleSix Blank Blank

quadMapping :: CharShape -> DoubleSix
quadMapping (tl, tr, bl, br) =
    DoubleSix (halfMapping (combine tl bl)) (halfMapping (combine tr br))
  where
    combine t b = round ((fromIntegral t + fromIntegral b) / 2 :: Double)
    halfMapping v
      | v >= 216  = Six
      | v >= 180  = Five
      | v >= 144  = Four
      | v >= 108  = Three
      | v >= 72   = Two
      | v >= 36   = One
      | otherwise = Blank

novemDoubleSix :: Image Pixel8 -> Int -> [[DoubleSix]]
novemDoubleSix img chsWide = novemAsciify' img chsWide novemMapping

novemMapping :: Novemant -> DoubleSix
novemMapping nvm = DoubleSix (sixMapping leftInt) (sixMapping rightInt)
  where
    maxVal = 3*3 + 3*3 `div` 2
    leftColVal = sum $ map brightNessToInt (leftCol nvm)
    centerColVal = sum $ map brightNessToInt (centerCol nvm)
    rightColVal = sum $ map brightNessToInt (leftCol nvm)
    leftInt = (6 * (leftColVal + centerColVal `div` 2) ) `div` maxVal
    rightInt = (6 * (rightColVal + centerColVal `div` 2) ) `div` maxVal
    sixMapping :: Int -> Section
    sixMapping i = case i of
                     0 -> Blank
                     1 -> One
                     2 -> Two
                     3 -> Three
                     4 -> Four
                     5 -> Five
                     _ -> Six
      where
        clamp = max 0 (min 6 i)

leftCol :: Novemant -> [Brightness]
leftCol (Novemant tl _ _ ml _ _ bl _ _) = [tl, ml, bl]

centerCol :: Novemant -> [Brightness]
centerCol (Novemant _ tc _ _ mc _ _ bc _) = [tc, mc, bc]

rightCol ::  Novemant -> [Brightness]
rightCol (Novemant _ _ rl _ _ mr _ _ br) = [rl, mr, br]

brightNessToInt :: Brightness -> Int
brightNessToInt Bnk = 3
brightNessToInt Lgt = 2
brightNessToInt Drk = 1
brightNessToInt Bck = 0


