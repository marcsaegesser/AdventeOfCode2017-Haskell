module Day2 where

import           Control.Applicative
import           Data.List
import           Data.Scientific (floatingOrInteger)
import qualified Data.ByteString.Char8 as BS
import           Data.Attoparsec.ByteString.Char8


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

parser :: Parser [[Int]]
parser = sepBy1 (sepBy1 parseNum spaceOrTab) endOfLine
  where
    parseNum = fmap toInt scientific
    spaceOrTab = char ' ' <|> char '\t'
    toInt s  = either (error "Invalid input") id $ floatingOrInteger s

withInput :: FilePath -> Parser a -> IO a
withInput path p = do
  f <- BS.readFile path
  let result = parseOnly p f
  either (error . show) return result

day02 :: IO ()
day02 =
  withInput "data/Day2.txt" parser >>= \parsed -> do
    putStrLn "Problem 1"
    print $ prob1 parsed
    putStrLn "Problem 2"
    print $ prob2 parsed
