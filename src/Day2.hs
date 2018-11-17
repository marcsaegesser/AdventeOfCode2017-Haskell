module Day2 where

import Data.List

prob1 :: [[Int]] -> Int
prob1 m = sum $ map maxDiff m
  where maxDiff l = maximum l - minimum l

prob2 :: [[Int]] -> Int
prob2 m = sum $ map rowValue m
  where rowValue r = product $ map findMatch $ ps r
        ps l = [(x,y) | (x:ys) <- tails l, y <- ys]
        findMatch (x, y)
          | rem x y == 0 = div x y
          | rem y x == 0 = div y x
          | otherwise    = 1

mkRow :: String -> [Int]
mkRow l = map (\n -> read n::Int) $ words l

loadPuzzle :: FilePath -> IO [[Int]]
loadPuzzle f = do
  ll <- fmap lines $ readFile f
  return $ map mkRow ll
