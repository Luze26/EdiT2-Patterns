module Util where


-- Activity
type Activity = String

-- Group
type Group = String

-- Participant
type Participant = String

-- Role
type Role = String

-- Resource
type Resource = String



-- readInt, read an int
-- @String -> the number
-- @Int -> int value
readInt :: String -> Int
readInt n = read n



-- Helpers to create automatic list of students //////////////////////////////////////////////////////

-- createList, create a list a participants for test
-- @Int -> number of participants to create
-- @String -> list of participants
createList :: Int -> String
createList n = '[' : createList' n ++ "]"



-- createList', create a list a participants for test
-- @Int -> number of participants to create
-- @String -> list of participants
createList' :: Int -> String
createList' 1 = "\"e1\""
createList' n = ('\"':'e':(show n)++"\",") ++ createList' (n-1)



-- Functions to write .t2 elements ///////////////////////////////////////////////////////////////////

scriptName :: String -> String
scriptName name = "@scriptName = " ++ name ++ "\n"

objectList :: [(String, [String])] -> String
objectList objects = foldl (\acc x -> acc ++ objectList' x) "" objects

objectList' :: (String, [String]) -> String
objectList' (name, list) = "objectList@" ++ name ++ "[" ++ (objectList'' list) ++ "]\n"

objectList'' :: [String] -> String
objectList'' [] = ""
objectList'' (x:[]) = "{\"" ++ x ++ "\", \"" ++ x ++ " description\"}"
objectList'' (x:xs) = "{\"" ++ x ++ "\", \"" ++ x ++ " description\"}," ++ objectList'' xs


teacherNotes :: Int -> String
teacherNotes nb = concat $ replicate nb "@teacherNotes = \"\"\n"



-- Helpers to split participants in groups ////////////////////////////////////////////////////////////

-- splitParticipants, split participants into group of size given by the list
-- @[Participants] -> participants
-- @[Int] -> list of size for groups
-- @[[Participant]] -> partcipants splited into groups
splitParticipants :: [a] -> [Int] -> [[a]]
splitParticipants [] _ = []
splitParticipants participants (n:numbers) = first : (splitParticipants rest numbers)
	where
		(first, rest) = splitAt n participants



-- repartition, give a list of group size, for the best repartition
-- @Int -> number of participants
-- @Int -> number of participants per group wanted
-- @Int -> above margin tolerated
-- @Int -> below margin tolerated
-- @[Int] -> size repartion
repartition :: Int -> Int -> Int -> Int -> [Int]
repartition nbP n a b
	| ok = list1
	| otherwise = list2
	where
		(ok, list1) = repartitionUniform nbP n a b 0
		(_, list2) = repartition' nbP n a b 0



-- repartition, list of number of participants per group
-- @Int -> number of participants
-- @Int -> number of participants per group wanted
-- @Int -> above margin tolerated
-- @Int -> below margin tolerated
-- @(Bool, [Int]) -> if possible Bool = True and size repartion
repartition' :: Int -> Int -> Int -> Int -> Int -> (Bool, [Int])
repartition' nbP n a b m
	| sub > 0 = let (ok, list) = repartition' sub n a b m in if ok then (ok, nb:list) else decrease
	| sub == 0 = (True, [nb])
	| otherwise = decrease
	where
		nb = n-m
		sub = nbP-nb
		decrease = if m<b && nb>2 then (repartition' nbP n a b (m+1)) else (False, [])



-- repartitionUniform, best repartition possible
-- @Int -> number of participants
-- @Int -> number of participants per group wanted
-- @Int -> above margin tolerated
-- @Int -> below margin tolerated
-- @(Bool, [Int]) -> if possible Bool = True and size repartion
repartitionUniform :: Int -> Int -> Int -> Int -> Int -> (Bool, [Int])
repartitionUniform nbP n a b m
	| m<=a && mod nbP nb == 0 = (True, replicate (div nbP nb) nb)
	| m<=b && nb>1 && mod nbP nb1 == 0 = (True, replicate (div nbP nb1) nb1)
	| m<a || m<b = repartitionUniform nbP n a b (m+1)
	| otherwise = (False, [])
	where
		nb = n+m
		nb1 = n-m 