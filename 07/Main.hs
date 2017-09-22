module Main where

import Data.Char (isLetter)
import Data.List (reverse)
import Data.List.Split (divvy)
import System.Environment (getArgs)
import Text.Regex.PCRE

hypernetRegex :: String
hypernetRegex = "\\[[a-z]+\\]"

textRegex :: String
textRegex = "(^|\\])[a-z]+($|\\[)"

extractSequences :: String -> String -> [String]
extractSequences ipString regex =
  map (filter isLetter) (getAllTextMatches $ ipString =~ regex)

reversiblePairInAnySequence :: [String] -> Bool
reversiblePairInAnySequence allSequences = True `elem` sequenceResults
  where
    sequenceResults = map reversePairInSingleSequence allSequences

reversePairInSingleSequence :: String -> Bool
reversePairInSingleSequence singleSequence = any isABBA sequenceSubStrings
  where
    sequenceSubStrings = divvy 4 1 singleSequence

isABBA :: String -> Bool
isABBA ipString = ipString == reverse ipString && ipString !! 0 /= ipString !! 1

supportsTLS :: String -> Bool
supportsTLS ipString =
  reversiblePairInAnySequence textSequences &&
  not (reversiblePairInAnySequence hypernetSequences)
  where
    hypernetSequences = extractSequences ipString hypernetRegex
    textSequences = extractSequences ipString textRegex

main :: IO ()
main = do
  [filename] <- getArgs
  contents <- readFile filename
  let ipsWithTLS = filter supportsTLS (lines contents)
  print (length ipsWithTLS)
