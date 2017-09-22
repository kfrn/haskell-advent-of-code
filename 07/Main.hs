module Main where

import Data.Char (isLetter)
import Data.List (reverse)
import Data.List.Split (divvy)
import System.Environment (getArgs)
import Text.Regex.PCRE

extractHypernetSequences :: String -> [String]
extractHypernetSequences ipString =
  map (filter isLetter) (getAllTextMatches $ ipString =~ "\\[[a-z]+\\]")

extractTextSequences :: String -> [String]
extractTextSequences ipString =
  map (filter isLetter) (getAllTextMatches $ ipString =~ "(^|\\])[a-z]+($|\\[)")

reversiblePairInAnySequence :: [String] -> Bool
reversiblePairInAnySequence allSequences = True `elem` sequenceResults
  where
    sequenceResults = map reversePairInSingleSequence allSequences

reversePairInSingleSequence :: String -> Bool
reversePairInSingleSequence singleSequence =
  True `elem` map isReversiblePair sequenceSubStrings
  where
    sequenceSubStrings = divvy 4 1 singleSequence

isReversiblePair :: String -> Bool
isReversiblePair ipString =
  ipString == reverse ipString && ipString !! 0 /= ipString !! 1

supportsTLS :: String -> Bool
supportsTLS ipString =
  reversiblePairInAnySequence textSequences &&
  not reversiblePairInAnySequence hypernetSequences
  where
    hypernetSequences = extractHypernetSequences ipString
    textSequences = extractTextSequences ipString

main :: IO ()
main = do
  [filename] <- getArgs
  contents <- readFile filename
  let ipsWithTLS = filter supportsTLS (lines contents)
  print (length ipsWithTLS)
