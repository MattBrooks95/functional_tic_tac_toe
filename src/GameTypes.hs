module GameTypes where

import Data.List (intercalate)

import BoardTypes (Board, makeBoard)
-- Player 1 | Player 2
data Player = XPlayer | OPlayer
instance Show Player where
	show player = case player of
		XPlayer -> "Player 1"
		OPlayer -> "Player 2"

data Move = Move { rowNumber :: Int, columnNumber :: Int, player :: Player } deriving Show

createMove :: Int -> Int -> Player -> Move
createMove rowNumber columnNumber player = (Move {rowNumber=rowNumber, columnNumber=columnNumber, player=player})

data Game = Game {
	board :: Board,
	player1 :: Player,
	player2 :: Player,
	moves :: [Move]
}

data GameResult = Player Player | Tie deriving Show

instance Show Game where
	show game = intercalate "\n" [show (board game), "player1: " ++ show (player1 game), "player2: " ++ show (player2 game), intercalate " " (map show (moves game))]
		
makeGame = Game { board = makeBoard, player1 = XPlayer, player2 = OPlayer, moves = [] }

getNextPlayer :: Game -> Player
getNextPlayer game = if null movesList
	then XPlayer
	else case lastMovePlayer of
		XPlayer -> OPlayer
		OPlayer -> XPlayer
	where
		movesList = moves game
		lastMovePlayer = (player (head (moves game)))

