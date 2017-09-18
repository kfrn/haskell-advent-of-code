module Main where

import System.Environment (getArgs)

diamondKeypad :: [[Char]]
diamondKeypad =
  [ ['-', '-', '1', '-', '-']
  , ['-', '2', '3', '4', '-']
  , ['5', '6', '7', '8', '9']
  , ['-', 'A', 'B', 'C', '-']
  , ['-', '-', 'D', '-', '-']
  ]

startingPosition :: (Int, Int)
startingPosition = (0, 2)

instructions :: [String]
instructions = ["RL", "RRDDD", "URRL", "LURRUL"]

data Direction
  = U
  | D
  | L
  | R
  deriving (Show)

type Moves = [(Int, Int)]

type Keypad = [[Char]]

getDirection :: Char -> Direction
getDirection 'U' = U
getDirection 'D' = D
getDirection 'L' = L
getDirection 'R' = R
getDirection _ = error "Bad direction"

invalidPosition :: Keypad -> (Int, Int) -> Bool
invalidPosition keypad (x, y)
  | x < 0 || x >= keypadWidth = True
  | y < 0 || y >= keypadWidth = True
  | otherwise = positionToChar keypad (x, y) == '-'
  where keypadWidth = length (head keypad)

newPosition :: Keypad -> (Int, Int) -> Direction -> (Int, Int)
newPosition keypad currentPosition direction =
  if invalidPosition keypad newPosition
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

endPositions :: Keypad -> (Int, Int) -> [[Direction]] -> [(Int, Int)]
endPositions keypad startPosition sequences =
  tail (scanl (applyMoves keypad) startPosition sequences)

applyMoves :: Keypad -> (Int, Int) -> [Direction] -> (Int, Int)
applyMoves keypad position directions = foldl (newPosition keypad) position directions

positionToChar :: Keypad -> (Int, Int) -> Char
positionToChar keypad (x, y) = keypad !! y !! x

main :: IO ()
main = do
  [filename] <- getArgs
  contents <- readFile filename
  let moves = lines contents
  let answer = solveProblem moves
  print answer

solveProblem :: [String] -> [Char]
solveProblem instructions = do
  let directions = instructionsToDirections instructions
  let endLocations = endPositions diamondKeypad startingPosition directions
  map (positionToChar diamondKeypad) endLocations
