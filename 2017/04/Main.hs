module Main where

import Data.Char (isLetter)
import Data.List (elemIndex, nub, sort, sortBy)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import System.Environment (getArgs)
import Text.Regex.PCRE

data Room = Room
  { encryptedName :: String
  , sectorID :: Int
  , decryptedName :: String
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
makeRoom roomString = do
  let sectorID = extractSectorID roomString
  let encryptedName = extractEncryptedName roomString
  Room
    encryptedName
    sectorID
    (decryptName encryptedName sectorID)
    (extractChecksum roomString)

decryptName :: String -> Int -> String
decryptName encryptedName sectorID =
  map (\letter -> decryptLetter letter sectorID) encryptedName

decryptLetter :: Char -> Int -> Char
decryptLetter inputLetter sectorID = cycle alphabet !! newLetterIndex
  where
    newLetterIndex = currentLetterAlphabetIndex inputLetter + sectorID

currentLetterAlphabetIndex :: Char -> Int
currentLetterAlphabetIndex letter = fromJust (elemIndex letter alphabet)

alphabet :: [Char]
alphabet = ['a' .. 'z']

main :: IO ()
main = do
  [filename] <- getArgs
  contents <- readFile filename
  let rooms = map makeRoom (lines contents)
  mapM_ print rooms
