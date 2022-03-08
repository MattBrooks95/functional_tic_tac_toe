module Game where

import Text.Read (readMaybe)

import BoardTypes (
		Board,
		Board(..),
		getRow1,
		getRow2,
		getRow3,
		Space,
		Row(..),
		Space(..),
		getSpace1,
		getSpace2,
		getSpace3
	)

import GameTypes (
		Game,
		Game(..),
		board,
		moves,
		Move,
		createMove,
		rowNumber,
		Player,
		Player(XPlayer, OPlayer),
		player,
		columnNumber,
		getNextPlayer
	)

updateSpace :: Space -> Move -> Space
updateSpace space move = newSpaceValue
	where
		newSpaceValue = case player move of
			XPlayer -> X
			OPlayer -> O
--
-- TODO I technically need to handle column numbers past 4, because it's possible for a move
-- to hold that type. But it would probably be better to just find way to ensure that
-- a move's columnNumber can only be exactly 3
updateRow :: Row -> Move -> Row
updateRow row move =
	case columnNumber move of
		1 -> (Row (updateSpace (getSpace1 row) move) space2 space3)
		2 -> (Row space1 (updateSpace (getSpace2 row) move) space3)
		3 -> (Row space1 space2 (updateSpace (getSpace3 row) move))
	where
		space1 = getSpace1 row
		space2 = getSpace2 row
		space3 = getSpace3 row

-- Board Player Move -> New Board
-- you can't just update the board, you actually need to update the game itself
-- this doesn't actually make a Move object, it's the act of "making a move" in the game
makeMove :: Game -> Move -> Game
makeMove game move =
	case rowNumber move of
		--learned about this syntax from stacked overflow, it creates a new object
		--all of the fields except for the ones that you specificall declare to be
		--something new, are copied. This saves you from typing out the whole
		--constructor
		1 -> game { board = Board (updateRow row1 move) row2 row3, moves = move : (moves game) }
		2 -> game { board = Board row1 (updateRow row2 move) row3, moves = move : (moves game) }
		3 -> game { board = Board row1 row2 (updateRow row3 move), moves = move : (moves game) }
	where
		row1 = getRow1 oldBoard
		row2 = getRow2 oldBoard
		row3 = getRow3 oldBoard
		oldBoard = board game

data RowColInput = RowColInput { row :: Int, col :: Int } deriving Show


gameLoop :: Game -> IO (Maybe Player)
gameLoop game = do
	print game
	--userMove <- getMove
	----let userMove = getMove
	--print userMove
	----let moveData = parseMove userInput
	----case moveData of
	----	Just RowColInput -> 
	----	Nothing -> 
	--let nextPlayer = getNextPlayer game 
	----print ("next player:" ++ show nextPlayer ++ " # of moves:" ++ show (length (moves game)))
	--let move = createMove (row userMove) (col userMove) nextPlayer
	move <- getAndValidateMove game
	gameLoop (makeMove game move)
	where
		getAndValidateMove game = do
			userInput <- getMove
			print userInput
			let nextPlayer = getNextPlayer game
			let move = createMove (row userInput) (col userInput) nextPlayer
			if isLegalMove game move
			then return move
			else getAndValidateMove game

isLegalMove :: Game -> Move -> Bool
isLegalMove game move = True


getMove :: IO (RowColInput)
getMove = do
	putStrLn "Please enter your move. For example, to input row 1 column 1 you would type \"11\""
	userInput <- getLine
	print userInput
	let moveInput = parseMove userInput
	case moveInput of
		Just rowColInput ->
			if validateInput rowColInput
			-- I have to use the 'return' keyword here to make sure that rowColInput is still IO
			--then return rowColInput
			then return rowColInput
			else tryAgain "Invalid input"
		Nothing -> tryAgain "Invalid input"
	-- query the user for input again
	where tryAgain string = do
		putStrLn string
		getMove

validateInput :: RowColInput -> Bool
validateInput rowColInput = inRange inputRow && inRange inputCol
	where
		inRange = \x -> elem x [1..3]
		inputRow = row rowColInput
		inputCol = col rowColInput

parseMove :: String -> Maybe RowColInput
parseMove [] = Nothing
--parseMove (x:xs) = RowColInput { row = readMaybe [x] :: Int, col= read xs :: Int } 
parseMove (x:xs) = 
	case row of
		Just row ->
			case col of
				Just col -> return RowColInput { row = row, col = col }
				Nothing -> Nothing
		Nothing -> Nothing
	where
		row = readMaybe [x] :: Maybe Int
		col = readMaybe xs :: Maybe Int
	

