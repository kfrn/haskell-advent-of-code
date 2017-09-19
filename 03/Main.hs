module Main where

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
    -- NOTE: alternative filter FN is: (not . null)

-- triplesFromString :: String -> [Maybe Int]
triplesFromString :: String -> (Maybe Int, Maybe Int, Maybe Int)
triplesFromString inputString =
  arrayToTriple (map (\elem -> readMaybe elem :: Maybe Int) cleanTuple)
  where
    cleanTuple = cleanTextTriple inputString

validTriangle :: (Int, Int, Int) -> Bool
validTriangle (a, b, c) =
  if allSidesSameLength (a, b, c)
    then True
    else if longestSide (a, b, c) > sumShorterSides (a, b, c)
           then True
           else False

allSidesSameLength :: (Int, Int, Int) -> Bool
allSidesSameLength (a, b, c) = a == b && b == c

longestSide :: (Int, Int, Int) -> Int
longestSide (a, b, c) = max a (max b c)

sumShorterSides :: (Int, Int, Int) -> Int
sumShorterSides (a, b, c) = smallestNumber (a, b, c) + middleNumber (a, b, c)

smallestNumber :: (Int, Int, Int) -> Int
smallestNumber (a, b, c) = min a (min b c)

middleNumber :: (Int, Int, Int) -> Int
middleNumber (a, b, c)
  | twoSidesTheSameLength (a, b, c) = mostFrequentNumber (a, b, c)
  | otherwise = max a (min b c)

twoSidesTheSameLength :: (Int, Int, Int) -> Bool
twoSidesTheSameLength (a, b, c) = a == b || b == c || a == c

mostFrequentNumber :: (Int, Int, Int) -> Int
mostFrequentNumber (a, b, c)
  | a == b = a
  | a == c = a
  | otherwise = b

main :: IO ()
main = do
  [filename] <- getArgs
  contents <- readFile filename
  let triples = map triplesFromString (lines contents)
  print triples
  print (length triples)
