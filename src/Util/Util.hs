module Util where

import Tree
import Constraints

-- Types ////////////////////////////////////////////////////////////////////////

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


-- objects
type PatternObjects = (ActivityObjects, GroupObjects, ParticipantObjects, ResourceObjects, RoleObjects)
type ActivityObjects = [(Name,Description)]
type GroupObjects = [(Name,Description)]
type ParticipantObjects = [(Login,FirstName,Surname,Email,City,Country)]
type ResourceObjects = [(Name,Description,MoodleTableName,MoodleResourceID)]
type RoleObjects = [(Name,Description)]
type TeacherNotes = [String]


-- information
type Name = String
type Description = String
type Login = String
type FirstName = String
type Surname = String
type Email = String
type Country = String
type City = String
type MoodleTableName = String
type MoodleResourceID = String


-- Error
data Possible a = Possible a | NotPossible String deriving (Eq)



-- Util functions //////////////////////////////////////////////////////////////////////////////////////

-- readInt, read an int
-- @String -> the number
-- @Int -> int value
readInt :: String -> Int
readInt n = read n



-- participantsLogins, return the list of participants logins
-- @ParticipantObjects -> participants objects
participantsLogins :: ParticipantObjects -> [Participant]
participantsLogins ps = map (\(login,_,_,_,_,_) -> login) ps



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



-- Functions to write .t2 ///////////////////////////////////////////////////////////////////

-- writeT2, write the .t2 file
-- @String -> file path
-- @[String] -> notions
-- @NTree Cell -> the tree
-- @PatternObjects -> objects
-- @TeacherNotes -> notes
writeT2 :: String -> [String] -> NTree Cell -> PatternObjects -> TeacherNotes -> IO()
writeT2 file notions tree objects notes = writeFile file $ show notions ++ "\n\nscript=" ++
	(showTree tree) ++ "\n\n" ++ (showObjects objects) ++ "\n\nteacherNotes = " ++ (show notes)



-- writeT2Err, write the .t2 error file
-- @String -> file path
-- @[NotPossible String] -> errors
writeT2Err :: String -> [Possible a] -> IO()
writeT2Err file errs = writeFile file $ errors errs



-- showObjects, convert pattern objects in a string for .t2
-- @PatternObjects -> objects
-- @String -> objects for .t2
showObjects :: PatternObjects -> String
showObjects (a,g,p,r,ro) = "activityObjectsList = " ++ (show a) ++ "\ngroupObjectsList = " ++ (show g) ++
	"\nparticipantObjectsList = " ++ (show p) ++ "\nresourceObjectsList = " ++ (show r) ++ "\nroleObjectsList = " ++ (show ro)



-- errors, convert errors in a .t2 format
-- @[NotPossible String] -> errors
-- @String -> errors in .t2
errors :: [Possible a] -> String
errors err = "Error=[" ++ (errors' err True) ++ "]\n"



-- errors', cf errors
errors' :: [Possible a] -> Bool -> String
errors' [] _ = ""
errors' (NotPossible msg:es) first = (if (first) then "\"" else ",\"") ++ msg ++ "\"" ++ errors' es False



-- Functions to read .t2 ///////////////////////////////////////////////////////////////////

readHeader :: String -> String
readHeader content = head $ lines content



-- stripHeader, strip the header
-- @String -> the file content
-- @String -> the string without the header
stripHeader :: String -> String
stripHeader text = drop 7 $ concatMap (\l -> l ++ "\n") $ drop 2 $ lines text


-- stripFooter, return lines of the string without the footer
-- @[String] -> the file content in lines
-- @[String] -> all lines without footer's lines
stripFooter :: [String] -> [String]
stripFooter [] = []
stripFooter (l:ls)
	| l == "" = []
	| otherwise = l : (stripFooter ls)


readTree :: String -> IO (NTree Cell)
readTree file = do
	content <- readFile file
	return (read $ concat $ stripFooter $ lines $ stripHeader content)



-- Functions to read constraint

readConstraints :: String -> IO [Cstr]
readConstraints file = do
	content <- readFile file
	return (map (\l -> read l :: Cstr) (lines content))


-- Helpers to split lists ////////////////////////////////////////////////////////////

-- splitList, split list into group of size given by the list
-- @[a] -> list to split
-- @[Int] -> list of groups sizes
-- @[[a]] -> splitted list
splitList :: [a] -> [Int] -> [[a]]
splitList [] _ = []
splitList list (n:numbers) = first : (splitList rest numbers)
	where
		(first, rest) = splitAt n list



-- splitList2, split list into group of given size
-- @[a] -> list to split
-- @Int -> number of split
-- @Int -> size of split
-- @[[a]] -> splitted list
splitList2 :: [a] -> Int -> Int -> [[a]]
splitList2 [] _ _ = []
splitList2 list nb size = splitList2' (cycle list) nb size



-- splitList2', cf splitList2, expect that the list is now infinite
splitList2' :: [a] -> Int -> Int -> [[a]]
splitList2' _ 0 _ = []
splitList2' list nb size = take size list : splitList2' (drop size list) (nb-1) size



-- repartition, give a list of group size, for the best repartition
-- @Int -> number of participants
-- @Int -> number of participants per group wanted
-- @Int -> above margin tolerated
-- @Int -> below margin tolerated
-- @Possible [Int] -> size repartion
repartition :: Int -> Int -> Int -> Int -> Possible [Int]
repartition nbP n a b
	| ok = Possible list1
	| ok2 = Possible list2
	| otherwise = NotPossible "Can't do a good repartition"
	where
		(ok, list1) = repartitionUniform nbP n a b 0
		(ok2, list2) = repartition' nbP n a b 0



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