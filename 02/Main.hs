module Main where

import System.Environment (getArgs)

keypad :: [[Int]]
keypad = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

startingPosition :: (Int, Int)
startingPosition = (1, 1)

-- instructions :: [String]
-- instructions = ["ULL", "RRDDD", "LURDL", "UUUUD"]

data Direction = U | D | L | R deriving Show
type Moves = [(Int, Int)]

getDirection :: Char -> Direction
getDirection 'U' = U
getDirection 'D' = D
getDirection 'L' = L
getDirection 'R' = R
getDirection _ = error "Bad direction"

invalidPosition :: (Int, Int) -> Bool
invalidPosition (x, y)
  | x < 0 || x > 2 = True
  | y < 0 || y > 2 = True
  | otherwise = False

newPosition :: (Int, Int) -> Direction -> (Int, Int)
newPosition currentPosition direction =
  if invalidPosition newPosition
    then currentPosition
    else newPosition
  where
    newPosition = move currentPosition direction

move :: (Int, Int) -> Direction -> (Int, Int)
move (currentX, currentY) U = (currentX, currentY - 1)
move (currentX, currentY) D = (currentX, currentY + 1)
move (currentX, currentY) L = (currentX - 1, currentY)
move (currentX, currentY) R = (currentX + 1, currentY)

instructionsToDirections :: [String] -> [[Direction]]
instructionsToDirections sequences = map charsToMoves sequences

charsToMoves :: String -> [Direction]
charsToMoves chars = map getDirection chars

endPositions :: (Int, Int) -> [[Direction]] -> [(Int, Int)]
endPositions startPosition sequences = tail (scanl applyMoves startPosition sequences)

applyMoves :: (Int, Int) -> [Direction] -> (Int, Int)
applyMoves position directions = foldl newPosition position directions

positionToNumber :: (Int, Int) -> Int
positionToNumber (x, y) = keypad !! y !! x

main :: IO ()
main = do
  [filename] <- getArgs
  contents <- readFile filename
  let moves = lines contents
  let answer = solveProblem moves
  print answer

solveProblem :: [String] -> [Int]
solveProblem instructions = do
  let directions = instructionsToDirections instructions
  let endLocations = endPositions startingPosition directions
  map positionToNumber endLocations
