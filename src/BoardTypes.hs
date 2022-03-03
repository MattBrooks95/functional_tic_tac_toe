module BoardTypes where

import Data.List (intercalate)

data Space = X | O | Blank
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
 
makeBoard = Board makeRow makeRow makeRow
makeRow = Row Blank Blank Blank

