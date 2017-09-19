module Main where

import Data.List (sort)
import Data.List.Extra (trim)
import Data.List.Split (splitOn)
import System.Environment (getArgs)
import Text.Read (readMaybe)

arrayToTriple :: [a] -> (a, a, a)
arrayToTriple [b, c, d] = (b, c, d)

cleanTextTriple :: String -> [String]
cleanTextTriple inputString = map trim splitString
  where
    splitString = filter (\elem -> length elem > 0) $ splitOn " " inputString

triplesFromString :: String -> (Int, Int, Int)
triplesFromString inputString =
  arrayToTriple (map (\elem -> read elem :: Int) cleanTuple) -- NOTE: does not handle invalid cases
  where
    cleanTuple = cleanTextTriple inputString

validTriangle :: (Int, Int, Int) -> Bool
validTriangle (a, b, c) =
  if allSidesSameLength (a, b, c)
    then True
    else if sumShorterSides (a, b, c) > longestSide (a, b, c)
           then True
           else False

allSidesSameLength :: (Int, Int, Int) -> Bool
allSidesSameLength (a, b, c) = a == b && b == c

longestSide :: (Int, Int, Int) -> Int
longestSide (a, b, c) = max a (max b c)

sumShorterSides :: (Int, Int, Int) -> Int
sumShorterSides (a, b, c) = shortestSide (a, b, c) + middleSide (a, b, c)

shortestSide :: (Int, Int, Int) -> Int
shortestSide (a, b, c) = min a (min b c)

middleSide :: (Int, Int, Int) -> Int
middleSide (a, b, c) = sort [a, b, c] !! 1

twoSidesTheSameLength :: (Int, Int, Int) -> Bool
twoSidesTheSameLength (a, b, c) = a == b || b == c || a == c

main :: IO ()
main = do
  [filename] <- getArgs
  contents <- readFile filename
  let triples = map triplesFromString (lines contents)
  -- print triples
  let validTriangles = filter validTriangle triples
  print (length validTriangles)
