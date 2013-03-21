module RT_Generate where

import Util
import RT_Info( Information(..), Passage )
import Data.Char( intToDigit )


{-===========================================================================-}	
{-============================ GENERAL FUNCTIONS ============================-}	

-- Generate, entry point. Generate the file .t2
-- @Information -> data
-- @String -> the entire script
generate :: Information -> String
generate info = header ++ (genPassages info) ++ "]"



-- genPassages, create groups, and generate subtrees for passages
-- @Inforamtion -> data
-- @String -> subtrees under the root node
genPassages :: Information -> String
genPassages Information { participants = p, passages = pa, nbParticipantsPerGroup = nbPPG } = genPassages' pa 1 $ createGroups p nbPPG



-- genPassages', generate subtrees for passages
-- @[String] -> passages
-- @Int -> the id of the node in its subtree
-- @[[Participant]] -> groups
-- @String -> subtrees for passages
genPassages' :: [String] -> Int -> [[Participant]] -> String
genPassages' [] _ _ = ""
genPassages' _ _ [] = ""
genPassages' (pa:passages) id groups = (printNode 1 "Passage" id' "1" pa $ genGroups groups id' id 1) ++ genPassages' passages (id+1) groups
	where
		id' = '1':(intToString id)



-- genGroupes, generate subtrees for groups
-- @[[Participant]] -> groups
-- @String -> parent id
-- @Int -> numero of the passage
-- @Int -> the id of the node in its subtree
-- @String -> subtrees for groups
genGroups :: [[Participant]] -> String -> Int -> Int -> String
genGroups [] _ _ _ = ""
genGroups (g:groups) pid n id = (printNode 2 "Group" id' pid "" $ genRoles g id' n 1) ++ genGroups groups pid n (id+1)
	where
		id' = pid ++ intToString (id)



-- genRoles, generate subtrees for roles
-- @[Participant] -> participants for the group
-- @String -> parent id
-- @Int -> numero for passage
-- @Int -> the id of the node in its subtree
-- @String -> subtrees for roles
genRoles :: [Participant] -> String -> Int -> Int -> String
genRoles [] _ _ _ = ""
genRoles participants pid n id = (printNode 3 "Role" id' pid role $ genParticipants participants' id' 1) ++ genRoles remaining pid n (id+1)
	where
		id' = pid ++ intToString (id)
		i = mod (n-1) $ length participants
		role
			| id == 1 = "Teacher"
			| otherwise = "Learner"
		participants'
			| id == 1 = [participants !! i]
			| otherwise = participants
		remaining
			| id == 1 = removeItem participants i
			| otherwise = []



-- genParticipants, generate subtrees for participants
-- @[Participant] -> participants
-- @String -> parent id
-- @Int -> the id of the node in its subtree
-- @String -> subtrees for roles
genParticipants :: [Participant] -> String -> Int -> String
genParticipants [] _ _ = ""
genParticipants (p:participants) pid id = (printNode 4 "Participant" (pid ++ intToString id) pid p "") ++ genParticipants participants pid (id+1)



-- createGroups, split participants into groups
-- @[Participant] -> participants
-- @Int -> number of participants per group
-- @[[Participant]] -> groups
createGroups :: [Participant] -> Int -> [[Participant]]
createGroups [] _ = []
createGroups participants nbPPG = first : (createGroups rest nbPPG)
	where
		(first, rest) = splitAt nbPPG participants


{-===========================================================================-}	
{-============================ UTILITY FUNCTIONS ============================-}	

-- header, the header
-- @String -> the header, with the root node
header :: String
header = "[\"Passage\",\"Group\",\"Role\",\"Participant\"]\n\nscript = Node (\"Root\",\"1\",\"null\",[]) [\n"



-- printNode, create the strings representing a node
-- @Int -> number of tabulations
-- @String -> column name
-- @String -> node's id
-- @String -> parent's id
-- @String -> content
-- @String -> subtrees
-- @String -> the node
printNode :: Int -> String -> String -> String -> String -> String -> String
printNode nbTabs column id pid content subtrees = tabs ++ "Node (\"" ++ column ++ "\",\"" ++ id ++ "\",\"" ++ pid ++ "\",[" ++ content' ++ "]) [" ++ subtrees' ++ "],\n"
	where
		tabs = tab nbTabs
		content' = if (length content == 0) then "" else '\"':content ++ "\""
		subtrees' = if (length subtrees == 0) then "" else '\n':subtrees ++ tabs


-- tab, concat tabulations
-- @Int -> number of tabulations
-- @String -> tabulations
tab :: Int -> String
tab i
	| i>0 = '\t':(tab (i-1))
	| otherwise = ""



-- intToString, conversion to String
-- @Int -> number
-- @String -> conversion to String
intToString :: Int -> String
intToString i
	| i<10 = iToDigit
	| i>=10 = (intToString $ div i 10) ++ iToDigit
	| otherwise = ""
	where iToDigit = [intToDigit $ mod i 10]



-- removeItem, remove the item at index i
-- @[a] -> list
-- @Int -> index of the the item
-- @[a] -> list without item at index i
removeItem :: [a] -> Int -> [a]
removeItem [] _ = []
removeItem list i = take i list ++ (drop (i+1) list)
