module Game where

import BoardTypes (Board, Board(..), getRow1, getRow2, getRow3, Space, Row(..), Space(..), getSpace1, getSpace2, getSpace3)
import GameTypes (Move, rowNumber, Player, Player(XPlayer, OPlayer), player, columnNumber)

updateSpace :: Space -> Move -> Space
updateSpace space move = newSpaceValue
	where
		newSpaceValue = case player move of
			XPlayer -> X
			OPlayer -> O
--
-- TODO I techincally need to handle column numbers past 4, because it's possible for a move
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
-- this doesn't actually make a Move object, it's the act of "making a move" in the game
makeMove :: Board -> Move -> Board
makeMove board move =
	case rowNumber move of
		1 -> (Board (updateRow row1 move) row2 row3)
		2 -> (Board row1 (updateRow row2 move) row3)
		3 -> (Board row1 row2 (updateRow row3 move))
	where
		row1 = getRow1 board
		row2 = getRow2 board
		row3 = getRow3 board

