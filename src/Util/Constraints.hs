module Constraints
(
	Cmd,
	Cstr(..),
	Identificator(..),
	Identificators
) where



-- Cmd, used to represent a constraint command
type Cmd = String



-- used, to know where we must look
data Identificator = Label String | Content String | Identificator String String deriving (Show, Read, Eq)
type Identificators = [Identificator]

 
-- Cstr, type to represent a constraint
data Cstr = 
	Cstr {
		items :: Identificators, --List of items
		command :: Cmd, --what to check
		wher ::  Identificator --Under what
	} deriving (Show, Read)
