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
generate info = header ++ (genIntro info) ++ (genPassages info) ++ "\t]\n]"



-- genInfo, create the first activity
-- @Inforamtion -> data
-- @String -> subtrees under the root node
genIntro :: Information -> String
genIntro Information { participants = p, passages = _, nbParticipantsPerGroup = _, above = _, below = _ } = genParticipants p "1" 1  2 ++ "\t],\n\tNode (\"Activity\",\"11\",\"1\",[\"Learning\"]) [\n"



-- genPassages, create groups, and generate subtrees for passages
-- @Inforamtion -> data
-- @String -> subtrees under the second activity
genPassages :: Information -> String
genPassages Information { participants = p, passages = pa, nbParticipantsPerGroup = nbPPG, above = a, below = b } = genPassages' pa 1 $ createGroups p nbPPG a b



-- genPassages', generate subtrees for passages
-- @[String] -> passages
-- @Int -> the id of the node in its subtree
-- @[[Participant]] -> groups
-- @String -> subtrees for passages
genPassages' :: [String] -> Int -> [[Participant]] -> String
genPassages' [] _ _ = ""
genPassages' _ _ [] = ""
genPassages' (pa:passages) id groups = (printNode 2 "Passage" id' "1" pa (genGroups groups id' id 1) $ passages==[]) ++ genPassages' passages (id+1) groups
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
genGroups (g:groups) pid n id = (printNode 3 "Group" id' pid "" (genRoles g id' n 1) $ groups==[]) ++ genGroups groups pid n (id+1)
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
genRoles participants pid n id = (printNode 4 "Role" id' pid role (genParticipants participants' id' 1 5) $ id>1) ++ genRoles remaining pid n (id+1)
	where
		id' = pid ++ intToString (id)
		i = mod (n-1) $ length participants
		(role, participants', remaining)
			| id == 1 = ("Teacher", [participants !! i], removeItem participants i)
			| otherwise = ("Learner", participants, [])



-- genParticipants, generate subtrees for participants
-- @[Participant] -> participants
-- @String -> parent id
-- @Int -> the id of the node in its subtree
-- @Int -> number of tabs
-- @String -> subtrees for participants
genParticipants :: [Participant] -> String -> Int -> Int -> String
genParticipants [] _ _ _ = ""
genParticipants (p:participants) pid id tabs = (printNode tabs "Participant" (pid ++ intToString id) pid p "" $ participants==[]) ++ genParticipants participants pid (id+1) tabs



-- createGroups, split participants into groups
-- @[Participant] -> participants
-- @Int -> number of participants per group
-- @Int -> above
-- @Int -> below
-- @[[Participant]] -> groups
createGroups :: [Participant] -> Int -> Int -> Int -> [[Participant]]
createGroups [] _ _ _ = []
createGroups participants nbPPG a b = splitPaticipants participants $ repartition (length participants) nbPPG a b 0
	where (ok, list) = repartition (length participants) nbPPG a b 0
		| ok = splitParticipants participants list
		| otherwise = []

splitParticipants :: [Participant] -> [Int] -> [[Participant]]
splitParticipants [] _ = []
splitParticipants participants n:numbers = first : (splitParticipants rest numbers)
	where
		(first, rest) = splitAt n participants

-- repartition, list of number of participants per group
-- @Int -> number of participants per group
-- @Int -> above
-- @Int -> below
-- @(Bool, [Int]) -> tuple, True if it's possible, [Int] the list
repartition :: Int -> Int -> Int -> Int -> Int -> (Bool, [Int])
repartition nbP n a b m
	| sub > 0 = let (ok, list) = repartition sub n a b m in if ok then (ok, nb:list) else decrease
	| sub == 0 = (True, [nb])
	| otherwise = decrease
	where
		nb = n-m
		sub = nbP-nb
		decrease = if m<b && nb>2 then (repartition nbP n a b (m+1)) else (False, [])

{-===========================================================================-}	
{-============================ UTILITY FUNCTIONS ============================-}	

-- header, the header
-- @String -> the header, with the root node
header :: String
header = "[\"Activity\",\"Passage\",\"Group\",\"Role\",\"Participant\"]\n\nscript = Node (\"Root\",\"1\",\"null\",[]) [\n\tNode (\"Activity\",\"11\",\"1\",[\"Introduction\"]) [\n"



-- printNode, create the strings representing a node
-- @Int -> number of tabulations
-- @String -> column name
-- @String -> node's id
-- @String -> parent's id
-- @String -> content
-- @String -> subtrees
-- @Bool -> if there is a node following
-- @String -> the node
printNode :: Int -> String -> String -> String -> String -> String -> Bool -> String
printNode nbTabs column id pid content subtrees remain = tabs ++ "Node (\"" ++ column ++ "\",\"" ++ id ++ "\",\"" ++ pid ++ "\",[" ++ content' ++ "]) [" ++ subtrees' ++ "]" ++ end
	where
		tabs = tab nbTabs
		content' = if (length content == 0) then "" else '\"':content ++ "\""
		subtrees' = if (length subtrees == 0) then "" else '\n':subtrees ++ tabs
		end
			| remain == True = "\n"
			| otherwise = ",\n" 


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
