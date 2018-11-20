module Day3 where

import qualified Data.Vector as V

data Coord = Coord Int Int deriving Show  -- Row and Column

addCoords :: Coord -> Coord -> Coord
addCoords (Coord r1 c1) (Coord r2 c2) = Coord (r1+r2) (c1+c2)

ringSize :: Int -> Int
ringSize 0 = 1
ringSize r = r*8

ringBase :: Int -> Int
ringBase r = sum $ map ringSize [0..r-1]

indexToRingAndOffset :: Int -> (Int, Int)
indexToRingAndOffset idx =
  let (r, b) = last $ takeWhile (\x -> snd x < idx) $ map (\ring -> (ring, ringBase ring)) [0..]
  in
    (r, idx - b - 1)

ringAndOffsetToAddr :: Int -> Int -> Int
ringAndOffsetToAddr r o = (ringBase r) + o

indexToCoord :: Int -> Coord
indexToCoord 1 = Coord 0 0
indexToCoord idx =
  let
    (ring, offset) = indexToRingAndOffset idx
    sideLength     = ring * 2
    (side, cell)   = quotRem offset sideLength
    startCell      = ring - 1
  in
    case side of
      0 -> Coord (cell-startCell) ring
      1 -> Coord ring (startCell-cell)
      2 -> Coord (startCell - cell) (-ring)
      3 -> Coord (-ring) (cell - startCell)
      _ -> error "Oops"

coordToIndex :: Coord -> Int
coordToIndex (Coord row col) = (ringBase ring) + offset + 1
  where
    ring       = max (abs row) (abs col)
    sideLength = ring * 2
    startCell  = ring - 1
    offset     = case (row, col) of
                   (0, 0) -> 0
                   (r, c) | r == (-ring) -> c + startCell + 3*sideLength   -- BottomSide
                   (r, c) | c == ring    -> r + startCell                  -- RightSide
                   (r, c) | r == ring    -> startCell - c + sideLength     -- TopSide
                   (r, c) | c == (-ring) -> startCell - r + 2*sideLength   -- LeftSide
                   _                     -> error "Invalid Coord"


stepsForIndex :: Int -> Int
stepsForIndex idx =
  let (Coord r c) = indexToCoord idx
  in
    abs r + abs c

adjacencies :: Coord -> [Coord]
adjacencies coord =
  let currIndex = coordToIndex coord
      adjCells = [Coord r c | r <- [-1..1], c <- [-1..1], r /= c || r /= 0]
      adj       = map (\c -> addCoords coord c)
      withIdx   = map (\c -> (coordToIndex c, c))
      avail     = filter (\(i, _) -> i < currIndex)
  in
    map snd $ (avail . withIdx . adj) adjCells

stressTest :: Int -> Int
stressTest value =
  helper (V.fromList [0, 1]) 2
  where
    helper :: (V.Vector Int) -> Int -> Int
    helper v i =
      let nextValue = sum $ map (\c -> v V.! (coordToIndex c)) $ adjacencies (indexToCoord i)
      in
        if nextValue > value then nextValue
                             else helper (V.snoc v nextValue) (i+1)

day03 :: IO ()
day03 = do
  putStrLn "Part 1"
  print $ stepsForIndex 347991
  putStrLn "Part 2"
  print $ stressTest 347991
