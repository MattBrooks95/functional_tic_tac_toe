module BoardTypes where

import Data.List (intercalate, concatMap, take)

data Space = X | O | Blank deriving Eq
instance Show Space where
	show space = case space of
		X -> "X"
		O -> "O"
		Blank -> " "

-- you can only use data for new "algebraic" types
-- you are expected to provide the data constructors here,
-- and therefore you can't use data constructors to declare that
-- "this is an array"
--data Board = [Space] deriving (Show)
--so, use type
data Row = Row Space Space Space
instance Show Row where
	show (Row sp1 sp2 sp3) = show sp1 ++ " | " ++ show sp2 ++ " | " ++ show sp3

isRowFull :: Row -> Bool
--isRowFull row = length blankSpaces > 0 -- it told me to use not null here
-- but, I needed to make sure that the list was empty, anyway, whoops
isRowFull row = null blankSpaces
	where
		-- there is a compiler flag for lambda-case that could make this nicer
		blankSpaces = filter (\x -> case x of { Blank -> True; _ -> False }) spaces
		--blankSpaces = filter (\case { Blank -> True; _ -> False } spaces
		spaces = getSpacesFromRow row


getSpacesFromRow :: Row -> [Space]
getSpacesFromRow row = [getSpace1 row, getSpace2 row, getSpace3 row]

-- the record syntax for this would be easier I suppose
-- if it was just a list of spaces, the type system wouldn't stop someone
-- from making a row that had 4 spaces -> I mean, that's more configurable, but it isn't tictactoe
getSpace1 (Row x _ _ ) = x
getSpace2 (Row _ x _ ) = x
getSpace3 (Row _ _ x) = x

data Board = Board Row Row Row
instance Show Board where
	-- intercalate is like ["a", "b", "c"].join(' ') -> "a b c"
	show (Board row1 row2 row3) = intercalate "\n" [show row1, rowLine, show row2, rowLine, show row3]
		where
			rowLine =  "---------"
getRow1 (Board x _ _) = x
getRow2 (Board _ x _) = x
getRow3 (Board _ _ x) = x

getSpacesFromBoard :: Board -> [Space]
getSpacesFromBoard board = concatMap getSpacesFromRow [
		getRow1 board,
		getRow2 board,
		getRow3 board
	]

--I was wondering if I can do something clever by just righting the logic to check
--each row in the grid, and then "slide" the elements around in such a way that the columns
--become rows, so I can just re-use the row logic
--inversing a matrix
threeInARow :: Space -> Board -> Bool
threeInARow space board =
	allEqualSpace space (take 3 spaces) -- top row
	|| allEqualSpace space (take 3 (drop 3 spaces)) -- middle row
	|| allEqualSpace space (take 3 (drop 6 spaces)) -- bottom row
	|| allEqualSpace space [spaces !! 0, spaces !! 3, spaces !! 6] --left column
	|| allEqualSpace space [spaces !! 1, spaces !! 4, spaces !! 7] --middle column
	|| allEqualSpace space [spaces !! 2, spaces !! 5, spaces !! 8] --right column
	|| allEqualSpace space [spaces !! 0, spaces !! 4, spaces !! 8] --top left to bottom right diagonal
	|| allEqualSpace space [spaces !! 2, spaces !! 4, spaces !! 6] --top right to bottom left diagonal
	where
		spaces = getSpacesFromBoard board
		allEqualSpace space spaces = length (filter (\x -> x == space) spaces) == length spaces

isBoardFull :: Board -> Bool
isBoardFull board = isRowFull row1 && isRowFull row2 && isRowFull row3
	where
		row1 = getRow1 board
		row2 = getRow2 board
		row3 = getRow3 board

makeBoard = Board makeRow makeRow makeRow
makeRow = Row Blank Blank Blank

