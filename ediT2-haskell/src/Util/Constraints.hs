-- | Module defining constraints format.
module Util.Constraints where



-- | 'Cmd', used to represent a constraint command.
type Cmd = String



-- | Used to designate nodes.
data Identificator
	= Label String		-- ^ Match node with the given label (CellLabel).
	| Content String	-- ^ Match node with the given content (CellComponents).
	| Identificator String String -- ^ Match node with the given label and content (CellLabel, CellComponents).
   deriving (Show, Read, Eq)



-- | List of Identificator.
type Identificators = [Identificator]

 

data Cstr =
	-- | 'Cstr', type to represent a constraint.
	Cstr {
		items :: Identificators, -- ^ List of items.
		command :: Cmd, -- ^ What to check.
		wher ::  Identificator -- ^ Where to look.
	}
	| CstrPattern String String
   deriving (Show, Read)



data Result = Result (Bool, [String]) | CstrBis Cstr



identificatorsToString :: Identificators -> [String]
identificatorsToString = map identificatorToString



identificatorToString :: Identificator -> String
identificatorToString i = case i of
	Label str -> "Notion " ++ str
	Content str -> str
	Identificator label str -> str ++ " of " ++ label