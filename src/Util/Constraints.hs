module Constraints
(
	Cmd,
	Cstr(..),
	Identificator(..),
	Item(..),
	Items
) where


-- Cmd, used to represent a constraint command
type Cmd = String

-- used, to know where we must look
data Identificator = Label String | Content String deriving (Show, Read)

data Item = Item String | PropContent deriving (Show, Read)
type Items = [Item]
 
-- Cstr, type to represent a constraint
data Cstr = 
	Cstr {
		items :: Items, --List of items
		command :: Cmd, --what to check
		wher ::  Identificator --Under what
	} deriving (Show, Read)
