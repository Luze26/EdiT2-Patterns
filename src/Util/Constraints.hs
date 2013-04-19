module Constraints
(
	Cmd,
	Cstr(..)
) where


-- Cmd, used to represent a constraint command
type Cmd = String

-- Cstr, type to represent a constraint
data Cstr = 
	Cstr {
		who :: ([String], Int),
		command :: Cmd,
		wher ::  (Int, [String], [String])
	} deriving (Show, Read)
