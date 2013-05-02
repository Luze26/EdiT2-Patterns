module Constraints
(
	Cmd,
	Cstr(..),
	Identificator(..)
) where


-- Cmd, used to represent a constraint command
type Cmd = String

data Identificator = Id Int | Label String deriving (Show, Read)

-- Cstr, type to represent a constraint
data Cstr = 
	Cstr {
		who :: [String], --List of items, level of the item
		command :: Cmd, --what to check
		wher ::  Identificator --Under what
	} deriving (Show, Read)
