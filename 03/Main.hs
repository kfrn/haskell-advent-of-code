module Main where

import Data.List (sort)
import Data.List.Extra (trim)
import Data.List.Split (splitOn)
import System.Environment (getArgs)
import Text.Read (readMaybe)

data Triangle = Triangle
  { shortestSide :: Int
  , middleSide :: Int
  , longestSide :: Int
  }

makeTriangle :: [Int] -> Triangle
makeTriangle [a, b, c] = Triangle a' b' c'
  where
    [a', b', c'] = sort [a, b, c]

triangleFromString :: String -> Triangle
triangleFromString inputString = makeTriangle (map read splitString)
  where
    splitString = map trim (filter (not . null) $ splitOn " " inputString)

-- Steve's syntax:
-- triangleFromString inputString = makeTriangle $ (read . trim) <$> splitString
--   where
--     splitString = filter (not . null) $ splitOn " " inputString

validTriangle :: Triangle -> Bool
validTriangle triangle =
  allSidesSameLength triangle || sumShorterSides triangle > longestSide triangle

allSidesSameLength :: Triangle -> Bool
allSidesSameLength (Triangle a b c) = a == b && b == c

sumShorterSides :: Triangle -> Int
sumShorterSides triangle = shortestSide triangle + middleSide triangle

main :: IO ()
main = do
  [filename] <- getArgs
  contents <- readFile filename
  let triangles = map triangleFromString (lines contents)
  -- print triples
  let validTriangles = filter validTriangle triangles
  print (length validTriangles)
