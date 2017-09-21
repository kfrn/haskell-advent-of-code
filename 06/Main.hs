module Main where

import Data.List (nub, sort, sortBy, transpose)
import Data.Ord (comparing)
import System.Environment (getArgs)

groupLetters :: String -> [String]
groupLetters inputString = do
  let uniques = nub (sort inputString)
  map (\targetLetter -> filter (== targetLetter) inputString) uniques

leastCommonLetter :: [[Char]] -> [Char]
leastCommonLetter listOfGroupedLetters = nub (head orderedByFrequency)
  where
    orderedByFrequency = sortBy (comparing length) listOfGroupedLetters

main :: IO ()
main = do
  [filename] <- getArgs
  contents <- readFile filename
  let columns = transpose (lines contents)
  let letters = map leastCommonLetter (map groupLetters columns)
  print (concat letters)
