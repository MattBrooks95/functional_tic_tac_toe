module Main where

import GameTypes (
		Game,
		Move,
		makeGame,
		getNextPlayer,
		createMove,
		Player(..),
		Player,
		createMove,
		moves
	)

import Game (makeMove)

--how do I make the game loop?
main :: IO ()
--main = putStr "Hello World"
main = do
	winner <- gameLoop makeGame
	case winner of
		Just player -> putStr (show player)
		_ -> putStr "The game was a tie"


gameLoop :: Game -> IO (Maybe Player)
gameLoop game = do
	print game
	userMove <- getMove
	--let userMove = getMove
	print userMove
	--let moveData = parseMove userInput
	--case moveData of
	--	Just RowColInput -> 
	--	Nothing -> 
	let nextPlayer = getNextPlayer game 
	print ("next player:" ++ show nextPlayer ++ " # of moves:" ++ show (length (moves game)))
	let move = createMove (row userMove) (col userMove) nextPlayer
	gameLoop (makeMove game move)

data RowColInput = RowColInput { row :: Int, col :: Int } deriving Show

parseMove :: String -> Maybe RowColInput
parseMove [] = Nothing
parseMove (x:xs) = Just RowColInput { row=read [x] :: Int, col= read xs :: Int } 
	
getMove :: IO (RowColInput)
getMove = do
	userInput <- getLine
	print userInput
	let moveInput = parseMove userInput in
		case moveInput of
			Just moveInput -> return moveInput
			Nothing -> getMove
