module Util.Util where

import Util.Tree
import Util.Constraints

-- Types ////////////////////////////////////////////////////////////////////////

-- | 'Activity'
type Activity = String

-- | 'Group'
type Group = String

-- | 'Participant'
type Participant = String

-- | 'Role'
type Role = String

-- | 'Resource'
type Resource = String


-- | 'PatternObjects' represents the list of objects for the bases notions
type PatternObjects = (ActivityObjects, GroupObjects, ParticipantObjects, ResourceObjects, RoleObjects)

-- | 'ActivityObjects'
type ActivityObjects = [(Name,Description)]

-- | 'GroupObjects'
type GroupObjects = [(Name,Description)]

-- | 'ParticipantObjects'
type ParticipantObjects = [(Login,FirstName,Surname,Email,City,Country)]

-- | 'ResourceObjects'
type ResourceObjects = [(Name,Description,MoodleTableName,MoodleResourceID)]

-- | 'RoleObjects'
type RoleObjects = [(Name,Description)]

-- | 'TeacherNotes'
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
data Possible a
	= Possible a	-- ^ If the action if possible, with the result.
	| NotPossible String	-- ^ If the action is impossible with a error message.
	deriving (Eq, Show)



-- Util functions //////////////////////////////////////////////////////////////////////////////////////

-- | 'readInt', read an int
readInt :: String -- ^ The number.
	-> Int -- ^ Int value.
readInt n = read n



-- | 'participantsLogins', return the list of participants logins.
participantsLogins :: ParticipantObjects -- ^ Participants objects.
	-> [Participant] -- ^ Participants logins.
participantsLogins ps = map (\(login,_,_,_,_,_) -> login) ps



-- | 'resourcesNames', returns the list of resources names.
resourcesNames :: ResourceObjects -- ^ Resources objects.
	-> [Resource] -- ^ Resources names.
resourcesNames rs = map (\(name,_,_,_) -> name) rs



-- | 'activitiesObjects', return the activities objects from a pattern object.
activitiesObjects :: PatternObjects -> ActivityObjects
activitiesObjects (o,_,_,_,_) = o


-- | 'groupsObjects', return the groups objects from a pattern object.
groupsObjects :: PatternObjects ->GroupObjects
groupsObjects (_,o,_,_,_) = o



-- | 'participantsObjects', return the participants objects from a pattern object.
participantsObjects :: PatternObjects -> ParticipantObjects
participantsObjects (_,_,o,_,_) = o



-- | 'resourcesObjects', return the resources objects from a pattern object.
resourcesObjects :: PatternObjects -> ResourceObjects
resourcesObjects (_,_,_,o,_) = o



-- | 'rolesObjects', return the roles objects from a pattern object.
rolesObjects :: PatternObjects -> RoleObjects
rolesObjects (_,_,_,_,o) = o



-- Helpers to create automatic list of students //////////////////////////////////////////////////////

-- | 'createList', Create a list a participants for test.
createList :: Int -- ^ Number of participants to be created
	-> String -- ^ List of participants.
createList n = show [ 'e' : (show i) | i <- [1..n]]



-- Functions to write .t2 ///////////////////////////////////////////////////////////////////

-- | 'writeT2', write the entire .t2 file.
writeT2 :: String -- ^ File path.
	-> [String] -- ^ List of notions names.
	-> NTree Cell -- ^ The tree.
	-> PatternObjects -- ^ Pattern objects.
	-> IO()
writeT2 file notions tree objects = writeFile file $ show notions ++ "\n\nscript=" ++
	(showTree tree) ++ "\n\n" ++ (showObjects objects) ++ "\n\nteacherNotes = " ++ (show $ replicate (nbLeaf tree) "")



-- | 'writeT2Err', write the .t2 error file.
writeT2Err :: String -- ^ File path.
	-> [Possible a] -- ^ List of errors.
	-> IO()
writeT2Err file errs = writeFile file $ errors errs



-- 'showObjects', convert pattern objects in a string for .t2
showObjects :: PatternObjects -- ^ Pattern objects.
	-> String -- ^ Pattern objects in .t2 format.
showObjects (a,g,p,r,ro) = "activityObjectsList = " ++ (show a) ++ "\ngroupObjectsList = " ++ (show g) ++
	"\nparticipantObjectsList = " ++ (show p) ++ "\nresourceObjectsList = " ++ (show r) ++ "\nroleObjectsList = " ++ (show ro)



-- | 'errors', convert errors in a .t2 format.
errors :: [Possible a] -- ^ Errors.
	-> String -- ^ Errors in .t2.
errors err = "Error=[" ++ (errors' err True) ++ "]\n"



-- 'errors'', cf 'errors'
errors' :: [Possible a] -- ^ Errors.
	-> Bool -- ^ If it's the first error.
	-> String -- ^ Errors in .t2.
errors' [] _ = ""
errors' (NotPossible msg:es) first = (if (first) then "\"" else ",\"") ++ msg ++ "\"" ++ errors' es False



-- Functions to read .t2 ///////////////////////////////////////////////////////////////////

-- | 'readHeader', return the first line of .t2 file.
readHeader :: String -- ^ .t2 content.
	-> String -- ^ First line of the .t2.
readHeader content = head $ lines content



-- | 'stripHeader', strip the header of .t2 content.
stripHeader :: String -- ^ .t2 content.
	-> String -- ^ .t2 content without the head (notions names + "script=").
stripHeader text = drop 7 $ concatMap (\l -> l ++ "\n") $ drop 2 $ lines text



-- 'stripFooter', return lines of the string without the footer.
stripFooter :: [String] -- ^ The file content in lines.
	-> [String] -- ^ All lines without footer's lines.
stripFooter [] = []
stripFooter (l:ls)
	| l == "" = []
	| otherwise = l : (stripFooter ls)



-- | 'readTree', read the tree from .t2.
readTree :: String -- ^ File path for .t2.
	-> IO (NTree Cell) -- ^ The tree stored in the .t2.
readTree file = do
	content <- readFile file
	return (read $ concat $ stripFooter $ lines $ stripHeader content)



-- Functions to read .txt information file ///////////////////////////////////////////////////////////////////

-- | 'readObjects', read informations from .txt information file.
readObjects :: String -- ^ File path to the information file.
	-> (String, PatternObjects) -- ^ (file path for output, pattern objects).
readObjects firstLine = read firstLine :: (String, PatternObjects)



-- Functions to read constraint

-- | 'readConstraints', read constraints stored in a file
readConstraints :: String -- ^ File path.
	-> IO [Cstr] -- ^ Constraints.
readConstraints file = do
	content <- readFile file
	return (map (\l -> read l :: Cstr) (lines content))


-- Helpers to split lists ////////////////////////////////////////////////////////////

-- | 'splitList', split a list into group of size given by the list.
splitList :: [a] -- ^ The list to split.
	-> [Int] -- ^ List of groups sizes.
	-> [[a]] -- ^ The list splitted.
splitList [] _ = []
splitList list (n:numbers) = first : (splitList rest numbers)
	where
		(first, rest) = splitAt n list



-- | 'splitList2', split a list into group of given size.
splitList2 :: [a] -- ^ The list to split.
	-> Int -- ^ Number of sublists.
	-> Int -- ^ Size of the split.
	-> [[a]] -- ^ The list splitted.
splitList2 [] _ _ = []
splitList2 list nb size = splitList2' (cycle list) nb size -- We create an infinite list.



-- 'splitList2\'', cf 'splitList2', expect that the list is now infinite
splitList2' :: [a] -> Int -> Int -> [[a]]
splitList2' _ 0 _ = []
splitList2' list nb size = take size list : splitList2' (drop size list) (nb-1) size



-- | 'possibleToList', convert 'Possible [a]' to '[a]'.
possibleToList :: Possible [a] -- ^ A possible action.
	-> [a] -- ^ The result of the action, [] if not possible.
possibleToList p = case p of NotPossible _ -> []; Possible list -> list



-- | 'repartition', give a list of group size, for the best repartition.
repartition :: Int -- ^ Number to split.
	-> Int -- ^ Size of a split wanted.
	-> Int -- ^ Above margin tolerated.
	-> Int -- ^ Below margin tolerated.
	-> Bool -- ^ True, if the user prefers splits uniform, no matter if there is a huge variation with the size wanted. False if he wants the solution the closest.
	-> Possible [Int] -- ^ Possible size repartition.
repartition nbP n a b uniform
	| ok && (uniform || (diff1 <= 1) || (diff1 <= diff2)) = Possible list1
	| ok2 = Possible list2
	| otherwise = NotPossible "Can't do a good repartition" -- If it can't do a repartition, it returns a 'NotPossible' result.
	where
		(ok, list1) = repartitionUniform nbP n a b 0 -- It tries to do an uniform repartition with exactly the same size for each splits.
		(ok2, list2) = repartition' nbP n a b 0 -- If there isn't any uniform repartition, it tries to create a repartion with less variations possible.
		diff1 = abs $ nbP - (head list1)
		diff2 = max (abs $ nbP - (minimum list2)) (abs $ nbP - (maximum list2))


-- | 'repartition'', tries to create a repartion with less variations possible.
repartition' :: Int -- ^ Number to split.
	-> Int -- ^ Size of a split wanted.
	-> Int -- ^ Above margin tolerated.
	-> Int -- ^ Below margin tolerated.
	-> Int -- ^ Variation.
	-> (Bool, [Int]) -- ^ If a repartition is possible then it returns (True, repartition) else (False, []).
repartition' nbP n a b m
	| sub > 0 = let (ok, list) = repartition' sub n a b m in if ok then (ok, nb:list) else decrease
	| sub == 0 = (True, [nb])
	| otherwise = decrease
	where
		nb = n-m
		sub = nbP-nb
		decrease = if m<b && nb>2 then (repartition' nbP n a b (m+1)) else (False, [])



-- | 'repartitionUniform', repartition with the same size for each group, and with the closest possible size of the size wanted.
repartitionUniform :: Int -- ^ Number of participants to split.
	-> Int -- ^ Number of participants per groups wanted.
	-> Int -- ^ Above margin tolerated.
	-> Int -- ^ Below margin tolerated.
	-> Int -- ^ Variation.
	-> (Bool, [Int]) -- ^ If a repartition is possible then it returns (True, repartition) else (False, []).
repartitionUniform nbP n a b m
	| m<=a && mod nbP nb == 0 = (True, replicate (div nbP nb) nb)
	| m<=b && nb>1 && mod nbP nb1 == 0 = (True, replicate (div nbP nb1) nb1)
	| m<a || m<b = repartitionUniform nbP n a b (m+1)
	| otherwise = (False, [])
	where
		nb = n+m
		nb1 = n-m



-- 'repartition2', give a list of size depending of split information, for the best repartition.
repartition2 :: Int -- ^ Number to split.
	-> [(Int, Int, Int)] -- ^ List of splits with for each split : the size wanted, the above margin and the below margin.
	-> Possible [Int] -- ^ Possible size repartition.
repartition2 nbP groups
	| sumSize == nbP = Possible sizeGroups
	| (diffSum > 0 && diffSum <= sumAbove) || (diffSum < 0 && diffSum >= (-sumBelow)) = Possible $ repartition2' nbP diffSum groups
	| otherwise = NotPossible "repartition not possible"
	where
		sizeGroups = map (\(nb,_,_) -> nb) groups
		(sumSize, sumAbove, sumBelow) = foldl (\(s,a,b) (s',a',b') -> (s+s',a+a',b+b')) (0,0,0) groups
		diffSum = nbP - sumSize



-- | 'repartition2'', cf 'repartition2', expect that 'repartition2'' is used only when there is a solution and the second parameter
-- take the difference between the number to split and the sum of sizes wanted
repartition2' :: Int -- ^ .
	-> Int -- ^ .
	-> [(Int, Int, Int)] -- ^ .
	-> [Int] -- ^ .
repartition2' _ _ [] = []
repartition2' nbP diff ((nbG,aG,bG):groups) = size : repartition2' (nbP-size) (diff-extra) groups
	where
		size = nbG + extra
		extra
			| diff > 0 && aG > 0 = min diff aG
			| diff < 0 && bG > 0 = max diff (-bG)
			| otherwise = 0