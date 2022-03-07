module Main where

import GameTypes (
--		Game,
		Move,
		makeGame,
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
	winner <- gameLoop makeGame
	case winner of
		Just player -> putStr (show player)
		_ -> putStr "The game was a tie"

