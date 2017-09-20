module Main where

import Data.Char (isLetter)
import Data.List (nub, sort, sortBy)
import System.Environment (getArgs)
import Text.Regex.PCRE

data Room = Room
  { encryptedName :: String
  , sectorID :: Int
  , checksum :: String
  } deriving (Show)

extractChecksum :: String -> String
extractChecksum roomString = filter isLetter (roomString =~ "\\[[a-z]{5}\\]")

extractSectorID :: String -> Int
extractSectorID roomString = read (roomString =~ "[0-9]{2,}")

extractEncryptedName :: String -> String
extractEncryptedName roomString =
  filter isLetter (roomString =~ "[a-z\\-]+\\-[0-9]")

makeRoom :: String -> Room
makeRoom roomString =
  Room
    (extractEncryptedName roomString)
    (extractSectorID roomString)
    (extractChecksum roomString)

groupLetters :: String -> [String]
groupLetters encryptedName = do
  let uniques = nub (sort encryptedName)
  map (\targetLetter -> filter (== targetLetter) encryptedName) uniques

orderByFrequency :: [[Char]] -> [[Char]]
orderByFrequency listOfGroupedLetters = sortBy commonToRare listOfGroupedLetters
  where
    commonToRare a b = length b `compare` length a

mostToLeastFrequentLetters :: String -> [[Char]]
mostToLeastFrequentLetters encryptedName = orderByFrequency groupedLetters
  where
    groupedLetters = groupLetters encryptedName

realRoom :: Room -> Bool
realRoom (Room encryptedName sectorID checksum) =
  fiveMostCommonLetters encryptedName == sort checksum

fiveMostCommonLetters :: String -> [Char]
fiveMostCommonLetters encryptedName =
  collapse (sort (take 5 (mostToLeastFrequentLetters encryptedName)))
  where
    collapse a = concatMap nub a

main :: IO ()
main = do
  [filename] <- getArgs
  contents <- readFile filename
  let rooms = map makeRoom (lines contents)
  let realRooms = filter realRoom rooms
  let realRoomSectorIDs = map (\room -> (sectorID room)) realRooms
  let sumSectorIDs = sum realRoomSectorIDs
  print sumSectorIDs
