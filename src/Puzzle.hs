{-# LANGUAGE LambdaCase,
             MultiWayIf,
             NoImplicitPrelude,
             OverloadedStrings,
             TemplateHaskell #-}
module Puzzle
  ( Puzzle
  ) where

import ClassyPrelude
import qualified Asciify as A
import Codec.Picture
import Data.List ((!!), tails)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import qualified Data.Vector as V
import Image.Generation
import System.Directory (createDirectoryIfMissing)

import Debug.Trace

data Puzzle a = Puzzle
  { cells :: V.Vector a
  , width :: Int
  } deriving Show

data Cell a = Cell
  { orientation :: Orientation
  , value :: a
  } deriving Show

asGrid :: Puzzle a -> [[a]]
asGrid (Puzzle c w) = map V.toList $ go c
  where
    go vec =
      if V.length vec == w
        then [vec]
        else
          case splitAt w vec of
            (h, rest) -> h : go rest

data Orientation = L | R | U | D deriving (Show, Eq)

data Option a = Option
  { opIdx :: Int
  , opCell :: Cell a
  }

data Options a = Options
  { optsOne :: Option a
  , optsTwo :: Option a
  , optsPenalty :: Integer
  }

greedy :: String -> Puzzle PixelRGB8 -> IO (Maybe (Puzzle (Cell PixelRGB8)))
greedy folderName act@(Puzzle c w) = do
    ref <- newIORef 0
    let go :: Puzzle (Maybe (Cell PixelRGB8)) -- The puzle state
           -> Set.Set Int -- set of indicies that are empty
           -> [Options PixelRGB8] -- Possible options available
           -> IO [Puzzle (Maybe (Cell PixelRGB8))]
        go st toFill opts = do
          modifyIORef ref (+1)
          num <- readIORef ref
          -- let img = generateFromMaybeGrid (map (map (fmap value)) (asGrid st))
          -- writePng (folderName ++ "/" ++ show num ++ ".png") img
          let valid = eachCellHasOneValidOption st opts
          -- contigSets = allContiguousSets st
              optSet = optionsIndicies opts
              onlyOne = findCellsWithOnlyOneOption opts
          if | null opts && valid -> return [st]
             | not (toFill `Set.isSubsetOf` optSet) -> do
                putStrLn $ "indicies left : " ++ tshow (length toFill)
                return []
            -- | any (\s -> odd (Set.size s)) contigSets ->
            --   trace ("odd set ") []
             | not (null onlyOne) -> do
                 go (applyOptions st onlyOne)
                    (deleteOptions toFill onlyOne)
                    (foldl' (\remain o -> filterConflictingOpts o remain) opts onlyOne)
             | otherwise -> do
                let loop :: [[Options PixelRGB8]]
                         -> IO [Puzzle (Maybe (Cell PixelRGB8))]
                    loop [] = return []
                    loop (curr:rest) = do
                      res <- case curr of
                               (o:nOpts) -> go (applyOption st o)
                                            (deleteOption toFill o)
                                            (filterConflictingOpts o nOpts)
                               _ -> return []
                      if null res
                        then loop rest
                        else return res
                loop (tails opts)
    results <- go emptyPuzzle (Set.fromList [0..len-1])
        (sortOptions $ allOptions act)
    return $ fullPuzzle =<< headMay results
  where
    emptyPuzzle = Puzzle (V.replicate len Nothing) w
    len = V.length c

allContiguousSets :: Puzzle (Maybe a) -> [Set.Set Int]
allContiguousSets puzz@(Puzzle c w) = go [] 0
  where
    go :: [Set.Set Int]
       -> Int
       -> [Set.Set Int]
    go prevSets i
      | i >= V.length c = prevSets
      | any (i `Set.member`) prevSets = go prevSets (i+1)
      | isNothing (c V.! i) = go prevSets (i+1)
      | otherwise = go (fanOut Set.empty i : prevSets) (i+1)
    fanOut :: Set.Set Int -> Int -> Set.Set Int
    fanOut currSet i
      | i >= V.length c = currSet
      | i `Set.member` currSet = currSet
      | isNothing (c V.! i) = currSet
      | otherwise =
        Set.unions $ map (fanOut (Set.insert i currSet)) (adjacent puzz i)

adjacent :: Puzzle a -> Int -> [Int]
adjacent (Puzzle c w) i
    | i < 0 = []
    | i >= V.length c = []
    | otherwise =
           (if i >= w then [i - w] else [])
        ++ (if i `div` w < h -1 then [i + w] else [])
        ++ (if i `rem` w /= 0 then [i - 1] else [])
        ++ (if i `rem` w /= w - 1 then [i + 1] else [])
  where
    h = V.length c `div` w

deleteOption :: Set.Set Int -> Options a -> Set.Set Int
deleteOption set (Options o1 o2 _) =
  Set.delete (opIdx o2) $ Set.delete (opIdx o1) set

deleteOptions ::Set.Set Int -> [Options a] -> Set.Set Int
deleteOptions set opts = foldl' deleteOption set opts

filterConflictingOpts :: Options a -> [Options a] -> [Options a]
filterConflictingOpts (Options (Option i1 _) (Option i2 _) _) opts =
  filter (\o -> let a = opIdx (optsOne o)
                    b = opIdx (optsTwo o)
                in a /= i1 && a /= i2 && b /= i1 && b /= i2
         ) opts

sortOptions :: [Options PixelRGB8] -> [Options PixelRGB8]
sortOptions = sortOn optsPenalty

groupByPenalty :: [Options PixelRGB8] -> [[Options PixelRGB8]]
groupByPenalty =
    groupBy (\a b -> optsPenalty a == optsPenalty b)
  . sortOptions

applyOption :: Puzzle (Maybe (Cell PixelRGB8))
            -> Options PixelRGB8
            -> Puzzle (Maybe (Cell PixelRGB8))
applyOption (Puzzle c w) (Options (Option i1 c1) (Option i2 c2) _) =
  Puzzle (c V.// [(i1, Just c1), (i2, Just c2)]) w

applyOptions :: Puzzle (Maybe (Cell PixelRGB8))
             -> [Options PixelRGB8]
             -> Puzzle (Maybe (Cell PixelRGB8))
applyOptions puzz opts = foldl' applyOption puzz opts

fullPuzzle :: Puzzle (Maybe (Cell PixelRGB8)) -> Maybe (Puzzle (Cell PixelRGB8))
fullPuzzle (Puzzle c w) =
  if all isJust c
     then Just $ Puzzle (map fromJust c) w
     else Nothing

eachCellHasOneValidOption :: Puzzle (Maybe (Cell PixelRGB8))
                          -> [Options PixelRGB8]
                          -> Bool
eachCellHasOneValidOption (Puzzle c _) opts =
    length (intersection idxsLeft optIdxs) == length idxsLeft
  where
    len = V.length c
    idxsLeft = Set.fromList $ filter (\i -> isNothing (c V.! i)) [0..(len-1)]
    optIdxs = Set.fromList $
      concatMap (\(Options (Option i1 _) (Option i2 _) _) -> [i1, i2]) opts

squaresLeft :: Puzzle (Maybe (Cell PixelRGB8)) -> Int
squaresLeft (Puzzle c _) = V.length $ V.filter isNothing c

optionIndices :: Options a -> [Int]
optionIndices (Options o1 o2 _) = [ opIdx o1, opIdx o2 ]

optionsIndicies :: [Options a] -> Set.Set Int
optionsIndicies opts = Set.fromList $ concatMap optionIndices opts

indicesLeft :: Puzzle (Maybe (Cell PixelRGB8)) -> Set.Set Int
indicesLeft (Puzzle c _) = Set.fromList $
  V.ifoldl' (\acc i v ->
               case v of
                 Nothing -> i : acc
                 Just _ -> acc
            ) [] c

{-
greedy2 :: Puzzle PixelRGB8 -> Maybe (Puzzle (Cell PixelRGB8))
greedy2 (Puzzle c w) =
  where
    emptyPuzzle = Puzzle (V.replicate len Nothing) w
    len = V.length c
    opts = allOptions puzz
    go :: Puzzle (Maybe (Cell PixelRGB8))
       -> [Options PixelRGB8]
       -> Puzzle (Maybe (Cell PixelRGB8))
    go st opts =
-}

allOptions :: Puzzle PixelRGB8 -> [Options PixelRGB8]
allOptions (Puzzle act w) =
    V.ifoldl' (\os idx _ ->
                let h = V.length act `div` w
                    onRight = (idx + 1) `rem` w == 0
                    onBottom = idx `div` w >= h - 1
                in (
                   if onRight then [] else
                       let l = act V.! (idx+1)
                           r = act V.! idx
                           (v, p) = findNearestOfTwo l r
                       in [ Options
                              { optsOne =
                                  Option
                                    { opIdx = idx
                                    , opCell = Cell R v
                                    }
                              , optsTwo =
                                  Option
                                    { opIdx = idx+1
                                    , opCell = Cell L v
                                    }
                              , optsPenalty = p
                              }
                          ]
                   ) ++
                   (
                       if onBottom then [] else
                           let t = act V.! idx
                               b = act V.! (idx+w)
                               (v, p) = findNearestOfTwo t b
                           in [ Options
                                  { optsOne =
                                      Option
                                        { opIdx = idx
                                        , opCell = Cell D v
                                        }
                                  , optsTwo =
                                      Option
                                        { opIdx = idx+w
                                        , opCell = Cell U v
                                        }
                                  , optsPenalty = p
                                  }
                              ]
                       ) ++ os
              ) [] act


fromRows :: [[a]] -> Maybe (Puzzle a)
fromRows rows@(x:xs)
    | any (\r -> length r /= w) xs = Nothing
    | otherwise = Just $
        Puzzle
          { cells = V.generate (w * (length rows))
                      (\idx ->
                         let (j,i) = idx `divMod` w
                         in (rows !! j) !! i
                      )
          , width = w
          }
  where
    w = length x

printGens :: Show a => a -> Int -> Int -> IO ()
printGens a w h =
  forM_ (generateAllPossibleOrientations a w h) $ \gen -> putStrLn (showGen (cells gen))

showGen :: Vector (Cell a) -> Text
showGen = pack . concat . V.toList . V.map (\c -> show $ orientation c)

generateAllPossibleOrientations :: a -> Int -> Int -> [Puzzle (Cell a)]
generateAllPossibleOrientations def w h =
    map (\vec -> Puzzle vec w) (go V.empty)
  where
    go vec
      | V.length vec == 0     =
                                concat [ go (V.singleton (Cell R def))
                                       , go (V.singleton (Cell D def))
                                       ]
      | V.length vec == w * h = [vec]
      | otherwise =
        let idx = V.length vec
            left = getOrientation vec (idx-1)
            above = getOrientation vec (idx-w)
            onRight = (idx + 1) `rem` w == 0
            onBottom = idx `div` w >= h - 1
        in if | left == Just R && above == Just D -> []
              | left == Just R ->
                  go (V.snoc vec (Cell L def))
              | above == Just D ->
                  go (V.snoc vec (Cell U def))
              | otherwise ->
                  concat [ if onRight then [] else go (V.snoc vec (Cell R def))
                         , if onBottom then [] else go (V.snoc vec (Cell D def))
                         ]
    getOrientation vec idx = orientation <$> vec V.!? idx

{-
optimizeOrientations :: Puzzle a
                     -> Puzzle (Cell a)
                     -> Puzzle (Cell a)
optimizeOrientations original p =
    go p 0 0
  where
    findN i = findNearest (cells original V.! i)
    go :: Puzzle (Cell a) -> Int -> (Puzzle (Cell a), Integer)
    go puzz@(Puzzle c w) idx =
      if idx >= V.length c
      then ( puzz
           , sum $ V.zipWith
             (\a (Cell _ v) -> rgbDiff a v)
             (cells original) c
           )
      else
      let results = concat
            [ case c V.! idx of
                Cell R v ->
                  let otherSectionIdx = idx + 1
                      otherColor = findNearest otherSectionIdx
                      colorChanged =
                        Puzzle { cells = c V.// [ (idx, otherColor)
                                                , (otherSectionIdx, otherColor)
                                                ]
                               , width = w
                               }
                      rotated =
                        Puzzle { cells =
                               ,  width = w
                               }
                  in
                Cell D v ->
                  let otherColor = findNearest (idx+w)
            , [go puzz (idx+1)]
            ]
      case sortOn snd results of
        ((result,_):_) -> result
        _ -> puzz
-}

makeCells :: Puzzle a -> Puzzle (Cell a)
makeCells (Puzzle c w)
    | w <= 1 = error "too narrow"
    | h <= 1 = error "too short"
    | odd w && odd h = error "Can't do odd grids"
    | otherwise =
      Puzzle
        { cells = V.generate (V.length c) (\idx ->
                    let (j,i) = idx `divMod` w
                        sideways = if even i then Cell R (c V.! idx)
                                   else Cell L (c V.! (idx - 1))
                    in if even w
                       then sideways
                       else if i /= w - 1
                               then sideways
                               else if even j then Cell D (c V.! idx)
                                    else Cell U (c V.! (idx - w))
                    )
        , width = w
        }
  where
    h = V.length c `div` w

getCell :: Puzzle a -> Int -> Int -> Maybe a
getCell (Puzzle c w) i j = c V.!? (j*w+i)

testColors :: String -> String -> IO ()
testColors inFile outFolder = do
  bs <- readFile inFile
  case A.decodeRGBImage bs of
    Left _ -> putStrLn "Failed"
    Right i -> do
      case fromRows (meanColors i 50) of
        Nothing -> putStrLn "Could not make puzzle"
        Just puzz -> do
          let folderName = "out/" ++ outFolder
          createDirectoryIfMissing True folderName
          result <- greedy folderName puzz
          case result of
            Nothing -> putStrLn "greedy failed"
            Just pResult -> do
              let grid = asGrid pResult
                  outImg = generateFromGrid (map (map value) grid)
              writePng (folderName ++ "/replica.png") outImg

findCellsWithOnlyOneOption :: [Options a] -> [Options a]
findCellsWithOnlyOneOption opts =
      concat . M.elems . M.filter (\os -> length os == 1) $ optMap
  where
    optMap = foldl' (\m o ->   M.insertWith (++) (opIdx (optsTwo o)) [o]
                             . M.insertWith (++) (opIdx (optsOne o)) [o]
                             $ m
                    ) M.empty opts

miniDominoColors :: [PixelRGB8]
miniDominoColors =
  [ PixelRGB8 255 255 255 -- white
  , PixelRGB8 128 128 128 -- gray
  , PixelRGB8 0 0 0 -- black
  , PixelRGB8 128 0 128 -- purple
  , PixelRGB8 187 50 121 -- fuscia
  , PixelRGB8 217 58 82 -- neon pink
  , PixelRGB8 255 0 0 -- red
  , PixelRGB8 255 165 0 -- orange
  , PixelRGB8 229 153 33 -- tangerine
  , PixelRGB8 255 255 0 -- yellow
  , PixelRGB8 139 240 63 -- neon green
  , PixelRGB8 0 255 0 -- green
  , PixelRGB8 108 188 187 -- teal
  , PixelRGB8 102 176 251 -- blue
  , PixelRGB8 45 65 147 -- royal blue
  ]

dominoColors :: [PixelRGB8]
dominoColors =
  [ PixelRGB8 255 255 255 -- white
  , PixelRGB8 0 0 0 -- black
  , PixelRGB8 18 32 145 -- dark blue
  , PixelRGB8 83 142 234 -- blue
  , PixelRGB8 165 210 230 -- light blue
  , PixelRGB8 183 219 230 -- ice
  , PixelRGB8 120 207 206 -- teal
  , PixelRGB8 118 189 72 -- green
  , PixelRGB8 151 245 56 -- neon green
  , PixelRGB8 160 196 35 -- sour apple green
  , PixelRGB8 252 248 52 -- neon yellow
  , PixelRGB8 240 199 41 -- yellow
  , PixelRGB8 232 203 83 -- pineapple
  , PixelRGB8 206 139 30 -- gold
  , PixelRGB8 194 51 21 -- neon orange
  , PixelRGB8 220 82 23 -- orange
  , PixelRGB8 204 54 23 -- red
  , PixelRGB8 205 68 50 -- maroon
  , PixelRGB8 202 60 82 -- neon pink
  , PixelRGB8 209 71 121 -- bubblegum pink
  , PixelRGB8 177 95 173 -- raspberry
  , PixelRGB8 167 147 227 -- sparkle lilac
  , PixelRGB8 160 130 203 -- lavendar
  , PixelRGB8 77 54 118 -- purple
  , PixelRGB8 36 45 70 -- navy blue
  , PixelRGB8 86 93 111 -- dark grey
  , PixelRGB8 160 170 187 -- grey
  , PixelRGB8 210 200 196 -- khaki
  , PixelRGB8 179 166 136 -- ivory
  , PixelRGB8 198 152 89 -- caramel
  , PixelRGB8 90 57 36 -- brown
  , PixelRGB8 136 136 107 -- olive drab
  ]

findNearest :: PixelRGB8 -> PixelRGB8
findNearest pix =
  fst . headEx . sortOn snd . map (\p2 -> (p2, rgbDiff pix p2)) $ dominoColors

findNearestOfTwo :: PixelRGB8 -> PixelRGB8 -> (PixelRGB8, Integer)
findNearestOfTwo p1 p2 =
  headEx . sortOn snd . map (\d -> (d, rgbDiff p1 d + rgbDiff p2 d)) $ dominoColors

rgbDiff :: PixelRGB8 -> PixelRGB8 -> Integer
rgbDiff (PixelRGB8 r1 g1 b1) (PixelRGB8 r2 g2 b2) = rd * rd + gd * gd + bd * bd
  where
    rd = fromIntegral r1 - fromIntegral r2
    gd = fromIntegral g1 - fromIntegral g2
    bd = fromIntegral b1 - fromIntegral b2

meanColors :: Image PixelRGB8 -> Int -> [[PixelRGB8]]
meanColors img width =
  -- TODO: this is janky
  take (length rows - 1) rows
  where
    rows =
      map (map (\((r,g,b), d) ->
                       if d == 0 then PixelRGB8 0 0 0 else toPixel (r/d,g/d,b/d))
          ) $
          A.makeGrid img 1 {- widthToHeight -}  width
          (\img topLeft diff ->
             A.linearCombinationInRect img topLeft diff
             (foldl' accum  ((0,0,0), 0))
          )
    accum :: ((Rational, Rational, Rational), Rational)
        -> (PixelRGB8, Rational)
        -> ((Rational, Rational, Rational), Rational)
    accum ((ra, ga, ba), den) (PixelRGB8 r g b, wgt) =
      ((fromIntegral r * wgt + ra, fromIntegral g * wgt + ga, fromIntegral b * wgt + ba)
      , den + wgt
      )
    toPixel (r,g,b) = PixelRGB8 (round r) (round g) (round b)

