module Main where

import Data.List (nub, sort, sortBy, transpose)
import Data.Ord (comparing)
import System.Environment (getArgs)

groupLetters :: String -> [String]
groupLetters inputString = do
  let uniques = nub (sort inputString)
  map (\targetLetter -> filter (== targetLetter) inputString) uniques

mostCommonLetter :: [[Char]] -> [Char]
mostCommonLetter listOfGroupedLetters = nub (head orderedByFrequency)
  where orderedByFrequency = sortBy (flip $ comparing length) listOfGroupedLetters

main :: IO ()
main = do
  [filename] <- getArgs
  contents <- readFile filename
  let columns = transpose (lines contents)
  let letters = map mostCommonLetter (map groupLetters columns)
  print (concat letters)
