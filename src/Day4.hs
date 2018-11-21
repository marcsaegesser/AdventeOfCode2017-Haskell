module Day4 where

import           Control.Applicative
import           Data.List
import qualified Data.ByteString.Char8 as BS
import           Data.Attoparsec.ByteString.Char8

isValidPassphrase :: [String] -> Bool
isValidPassphrase phrase =
  length (filter (\(x, y) -> x == y) pairs) == 0
  where sortedWords = sort phrase
        pairs = zip sortedWords (drop 1 (cycle sortedWords))

isValidPassphrase2 :: [String] -> Bool
isValidPassphrase2 phrase =
  length (filter (\(x, y) -> x == y) pairs) == 0
  where sortedSortedWords = sort $ map sort phrase
        pairs = zip sortedSortedWords (drop 1 (cycle sortedSortedWords))

countValid :: [[String]] -> Int
countValid ls = length $ filter isValidPassphrase ls

countValid2 :: [[String]] -> Int
countValid2 ls = length $ filter isValidPassphrase2 ls

parser :: Parser [[String]]
parser = sepBy1 (sepBy1 (many1 letter_ascii) spaceOrTab) endOfLine
  where
    spaceOrTab = char ' ' <|> char '\t'

withInput :: FilePath -> Parser a -> IO a
withInput path p = do
  f <- BS.readFile path
  let result = parseOnly p f
  either (error . show) return result


day04 :: IO ()
day04 =
  withInput "data/Day4.txt" parser >>= \parsed -> do
    putStrLn "Part 1"
    print $ countValid parsed
    putStrLn "Part 2"
    print $ countValid2 parsed

