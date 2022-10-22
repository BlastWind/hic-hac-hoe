module Main (main) where

import Lib

main :: IO ()
main = someFunc


alternateMove :: Cell -> Cell 
alternateMove O = X 
alternateMove X = O

-- Set board size 
-- Continously ask for where to set tile until win 
	-- if not empty, ask again 
	-- if empty, place move and alternate move

	