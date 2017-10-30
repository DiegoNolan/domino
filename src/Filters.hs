{-# LANGUAGE LambdaCase,
             MultiWayIf,
             NoImplicitPrelude,
             OverloadedStrings,
             TemplateHaskell #-}
module Filters
  ( invertGrayscale
  , flatDialate
  , sobelBinary
  , sobel
  , amplifyEdges
  , gaussianFilter
  ) where

import ClassyPrelude hiding ((<|))
import Codec.Picture
import Codec.Picture.Types

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
    hij :: Float -> Int -> Int -> Int -> Float
    hij sigma k i j = (1 / (2 * pi * var)) * exp inner
      where
        num = (fromIntegral i - fromIntegral k - 1) ^^ 2 +
              (fromIntegral j - fromIntegral k - 1) ^^ 2
        inner = negate $ num / (2 * var)
        var = sigma ^^ 2

flatDialate :: Int -> Image Pixel8 -> Image Pixel8
flatDialate k img = generateImage f (imageWidth img) (imageHeight img)
  where
    f x y = maximumEx $
            map (\(x', y') -> pixDef0 x' y')
              [ (x', y')
              | x' <- xs
              , y' <- ys
              ]
      where
        xs = map (+x) [(1-2*k)..(2*k-1)]
        ys = map (+y) [(1-2*k)..(2*k-1)]
    pixDef0 :: Int -> Int -> Pixel8
    pixDef0 x y
      | x < 0                = 0
      | y < 0                = 0
      | x >= imageWidth img  = 0
      | y >= imageHeight img = 0
      | otherwise            = pixelAt img x y

sobelBinary :: Double -- ^ (0,1)
            -> Image Pixel8
            -> Image Pixel8
sobelBinary tol img =
  pixelMap (\p -> if fromIntegral p / 255 < tol
                  then 255
                  else 0
           ) (sobel img)

sobel :: Image Pixel8 -> Image Pixel8
sobel image = clampFloatImage $ generateImage
              (\i j -> sqrt
                      ( (pixelAt gx i j) ** 2 + (pixelAt gy i j) ** 2 )
              ) (imageWidth image) (imageHeight image)
  where
    gx = sobelX image
    gy = sobelY image

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
