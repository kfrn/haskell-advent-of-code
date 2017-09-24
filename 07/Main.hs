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

isABA :: String -> Bool
isABA ipString = reversible && differentChars
  where
    reversible = ipString == reverse ipString
    differentChars = ipString !! 0 /= ipString !! 1

extractABAs :: String -> [String]
extractABAs singleSequence = filter isABA substrings
  where
    substrings = divvy 3 1 singleSequence

reverseABA :: String -> String
reverseABA aba = drop 1 aba ++ [aba !! 1]

supportsSSL :: String -> Bool
supportsSSL ipString = do
  let reverseABAs = map reverseABA (concatMap extractABAs hypernetSequences)
  let textSequenceBABs = concatMap extractABAs textSequences
  any (`elem` reverseABAs) textSequenceBABs
  where
    hypernetSequences = extractSequences ipString hypernetRegex
    textSequences = extractSequences ipString textRegex

main :: IO ()
main = do
  [filename] <- getArgs
  contents <- readFile filename
  let ipsWithSSL = filter supportsSSL (lines contents)
  print (length ipsWithSSL)
