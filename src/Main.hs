module Main where

import GameTypes (
--		Game,
		Move,
		makeGame,
		Player,
		Player(..),
		GameResult,
		GameResult(..),
--		getNextPlayer,
--		createMove,
--		Player(..),
--		Player,
--		createMove,
--		moves
	)

import Game (
		gameLoop,
	)

--how do I make the game loop?
main :: IO ()
--main = putStr "Hello World"
main = do
	gameResult <- gameLoop makeGame
	case gameResult of
		Player player -> print (show player ++ " wins!")
		Tie -> putStr "The game was a tie"

