module Main where

import Data.List (sort, transpose)
import Data.List.Split (divvy, splitOn)
import System.Environment (getArgs)

numberOfColumns :: Int
numberOfColumns = 3

data Triangle = Triangle
  { shortestSide :: Int
  , middleSide :: Int
  , longestSide :: Int
  }

makeTriangle :: [Int] -> Triangle
makeTriangle [a, b, c] = Triangle a' b' c'
  where
    [a', b', c'] = sort [a, b, c]

splitString :: String -> [String]
splitString inputString = filter (not . null) $ splitOn " " inputString

convertStringToInts :: [String] -> [Int]
convertStringToInts stringInt = map read stringInt

reorderColumnwise :: [[Int]] -> [Int]
reorderColumnwise inputList = concat (transpose inputList)

createSublist :: Int -> Int -> [Int]
createSublist initialIndex subListLength =
  take subListLength [initialIndex,initialIndex + numberOfColumns ..]

validTriangle :: Triangle -> Bool
validTriangle triangle =
  allSidesSameLength triangle || sumShorterSides triangle > longestSide triangle

allSidesSameLength :: Triangle -> Bool
allSidesSameLength (Triangle a b c) = a == b && b == c

sumShorterSides :: Triangle -> Int
sumShorterSides triangle = shortestSide triangle + middleSide triangle

findValidTriangles :: [Int] -> [Triangle]
findValidTriangles reorderedValues =
  filter validTriangle (map makeTriangle triangleArrays)
  where
    triangleArrays = divvy numberOfColumns numberOfColumns reorderedValues

main :: IO ()
main = do
  [filename] <- getArgs
  contents <- readFile filename
  let stringInts = map splitString (lines contents)
  let ints = map convertStringToInts stringInts
  if length (concat stringInts) `mod` numberOfColumns /= 0
    then putStrLn "The input isn't divisible by three, sorry - no can do!"
    else do
      let reorderedValues = reorderColumnwise ints
      print (length $ findValidTriangles reorderedValues)
