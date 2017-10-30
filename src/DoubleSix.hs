{-# LANGUAGE LambdaCase,
             MultiWayIf,
             NoImplicitPrelude,
             OverloadedStrings,
             TemplateHaskell #-}
module DoubleSix
  ( scaleDoubleSix
  , novemDoubleSix
  ) where

import ClassyPrelude hiding ((<|))
import Asciify hiding (Blank)
import Data.Aeson
import Data.List ((!!))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Vector as Vec
import qualified Data.Vector.Storable as V
import Debug.Trace
import qualified Asciify as A
import Codec.Picture
import Codec.Picture.Types
import Filters
import Image.Generation
import Shared.Domino.DoubleSix
import Shared.Domino.DoubleSix.Stats
import System.Directory (createDirectoryIfMissing)
import Text.PrettyPrint.HughesPJClass

widthToHeight :: Rational
widthToHeight = 2

testGeneration :: Double -> String -> String -> (Image Pixel8 -> Image Pixel8)-> IO ()
testGeneration sz inFile outFolder f = do
  bs <- readFile inFile
  case decodeGSImage bs of
    Left _ -> putStr "Failed"
    Right i -> do
      let rows = scaleDoubleSix (f i) (desiredWidthToCount sz)
          folderName = "out/" ++ outFolder
          ds = generateDoubleSixLayout rows
      createDirectoryIfMissing True folderName
      writePng (folderName ++ "/pattern.png") ds
      writePng (folderName ++ "/replica.png") $ invertGrayscale ds
      writeFile (folderName ++ "/stats.txt")
          ((encodeUtf8 . pack . prettyShow . getStats) rows)

{-
teststuff :: FilePath -> IO ()
testStuff inFile = do
  bs <- readFile inFile
  case decodeGSImage bs of
    Left _ -> putStr "Failed"
    Right i -> do
      let dat  = imageData i
          pMax = fromIntegral (V.foldl' (\acc i -> max acc i) minBound dat) :: Rational
          pMin = fromIntegral (V.foldl' (\acc i -> min acc i) maxBound dat) :: Rational
          cnts v = V.foldl' (\acc i -> if i == v then acc+1 else acc) 0 dat :: Int
          allCounts = map (\v -> (v,cnts v)) [0..255]
      putStrLn $ "max pixel : " ++ tshow pMax
      putStrLn $ "min pixel : " ++ tshow pMin
      forM_ allCounts $ \(v, c) ->
        putStrLn $ tshow v ++ "s : " ++ tshow c
-}

scaleDoubleSix :: Image Pixel8 -> Int -> [[DoubleSix]]
scaleDoubleSix img domsWide =
    -- TODO : This is janky
    take (length rows - 1) rows
  where
    rows = quadAsciify' img widthToHeight domsWide quadMapping

colorRegions :: Int -- ^ Max regions
             -> Image Pixel8 -- ^ Should be binary image
             -> Image Pixel8
colorRegions maxRegions img = generateImage f w h
  where
    regions = removeSmallRegions maxRegions $ findRegions img
    mx = Vec.maximum regions
    w = imageWidth img
    h = imageHeight img
    f x y = if mx == 0 then 0 else
      fromIntegral $ (255 * (regions Vec.! (y*w + x))) `div` mx

removeSmallRegions :: Int -> Vec.Vector Int -> Vec.Vector Int
removeSmallRegions maxRegions regions =
    if length elements <= maxRegions
      then regions
      else removeSmall
  where
    counts = foldl' (\mp p -> Map.insertWith (+) p 1 mp) Map.empty regions
    elements = Map.assocs counts
    regionsLeft =   Map.fromList
                  . take maxRegions
                  . sortBy (\(_,a) (_,b) -> compare b a)
                  $ elements
    mapping = Map.fromList $ zip (Map.keys regionsLeft) [1..]
    removeSmall = map (\v -> case Map.lookup v mapping of
                               Nothing -> 0
                               Just newValue  -> newValue
                      ) regions

findRegions :: Image Pixel8 -- ^ Show be either 255 or 0
            -> Vec.Vector Int
findRegions img = go (Vec.replicate (w*h) 0) 1 (0,0)
  where
-- https://en.wikipedia.org/wiki/Connected-component_labeling#One_component_at_a_time
    w = imageWidth img
    h = imageHeight img
    toIndex (x,y) = y*w + x
    nextPixel (x,y) = if | x == w - 1 && y == h - 1 -> Nothing
                         | x == w - 1 -> Just (0, y+1)
                         | otherwise  -> Just (x+1, y)
    go labels currLabel p@(x,y) =
      if pixelAt img x y == 255 && labels Vec.! (toIndex p) == 0
        then
             let new = doNeighbors (labels Vec.// [(toIndex p, currLabel)]) currLabel p
             in case nextPixel p of
                  Nothing -> new
                  Just np -> go new (currLabel+1) np
        else case nextPixel p of
               Nothing -> labels
               Just np -> go labels currLabel np
    doNeighbors labels currLabel p@(x,y) =
      let ns = neighbors x y
      in foldl' (\lbls neighbor@(i,j) ->
                   if pixelAt img i j == 255 && lbls Vec.! (toIndex neighbor) == 0
                     then doNeighbors (lbls Vec.// [(toIndex neighbor, currLabel)])
                                      currLabel neighbor
                     else lbls
                ) labels ns
    neighbors i j = filter (\(x,y) -> x >= 0 && x < w && y >= 0 && y < h &&
                                      not (x == i && y == j))
                           [ (i',j')
                           | i' <- [i-1,i,i+1]
                           , j' <- [j-1,j,j+1]
                           ]

testStuff :: String -> (Image Pixel8 -> a) -> IO a
testStuff inFile f = do
  bs <- readFile inFile
  case decodeGSImage bs of
    Left _ -> error "failed"
    Right i -> return (f i)

testAlg :: String -> String -> (Image Pixel8 -> Image Pixel8) -> IO ()
testAlg inFile outFile f = do
  bs <- readFile inFile
  case decodeGSImage bs of
    Left _ -> putStr "Failed"
    Right i -> writePng outFile (f i)

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
    combine t b = fromIntegral $ (t' + b') `div` 2
      where
        t' = fromIntegral t :: Int
        b' = fromIntegral b :: Int
    halfMapping v
      | v >= 216  = Six
      | v >= 180  = Five
      | v >= 144  = Four
      | v >= 108  = Three
      | v >= 72   = Two
      | v >= 36   = One
      | otherwise = Blank

novemDoubleSix :: Image Pixel8 -> Int -> [[DoubleSix]]
novemDoubleSix img chsWide = novemAsciify' img widthToHeight chsWide novemMapping

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


