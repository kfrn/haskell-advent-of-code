module Main where

import Data.Char (intToDigit)
import Data.Ix (range)
import Data.List (intersperse, transpose)
import Data.List.Split (divvy)
import Numeric (showIntAtBase)

puzzleInput :: Int
puzzleInput = 10 -- 1352

gridWidth :: Int
gridWidth = 9 -- 42

gridHeight :: Int
gridHeight = 6 -- 42

startPosition :: (Int, Int)
startPosition = (1, 1)

endPosition :: (Int, Int)
endPosition = (7, 4) -- (31, 39)

baseCalculation :: (Int, Int) -> Int
baseCalculation (y, x) =
  (x * x) + (3 * x) + (2 * x * y) + y + (y * y) + puzzleInput

intToBinary :: Int -> String
intToBinary int = showIntAtBase 2 intToDigit int ""

count1sInBinaryString :: String -> Int
count1sInBinaryString binaryString = length ones
  where
    ones = filter (\letter -> letter == '1') binaryString

totalOnes :: (Int, Int) -> Int
totalOnes coord = count1sInBinaryString (intToBinary baseValue)
  where
    baseValue = baseCalculation coord

cellValue :: (Int, Int) -> Char
cellValue coord =
  if odd (totalOnes coord)
    then '+' -- wall
    else ' '

replaceCharacterInString :: String -> Int -> Char -> String
replaceCharacterInString inputString index replacement =
  start ++ (replacement : end)
  where
    start = take index inputString
    end = drop (index + 1) inputString

replaceStringInList :: [String] -> (Int, Int) -> Char -> [String]
replaceStringInList inputStringList (x, y) replacement =
  firstStrings ++ modifiedString ++ lastStrings
  where
    firstStrings = take y inputStringList
    modifiedString =
      [replaceCharacterInString (inputStringList !! y) x replacement]
    lastStrings = drop (y + 1) inputStringList

chopUpList :: [(Int, Int)] -> [[(Int, Int)]]
chopUpList cellValues = divvy (gridWidth + 1) (gridWidth + 1) cellValues

gridWithPositions :: [String] -> [String]
gridWithPositions cellValues =
  replaceStringInList gridWithStart endPosition 'E' -- end marker
  where
    gridWithStart = replaceStringInList cellValues startPosition 'S' -- start marker

main :: IO ()
main = do
  let baseGrid = chopUpList (range ((0, 0), (gridHeight, gridWidth)))
  let cellValues = map (map cellValue) baseGrid
  let viewingGrid =
        map (\sublist -> intersperse ' ' sublist) (gridWithPositions cellValues)
  let dividingLine = replicate ((gridWidth * 2) + 1) '_'
  putStrLn dividingLine
  mapM_ putStrLn viewingGrid
  putStrLn dividingLine
