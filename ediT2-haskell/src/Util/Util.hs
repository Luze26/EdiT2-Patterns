module Util.Util where


import Util.Tree
import Util.Constraints



-- Error
data Possible a
	= Possible a	-- ^ If the action if possible, with the result.
	| NotPossible String	-- ^ If the action is impossible with a error message.
	deriving (Eq, Show)



-- Util functions //////////////////////////////////////////////////////////////////////////////////////

-- | 'readInt', read an int
readInt :: String -- ^ The number.
	-> Int -- ^ Int value.
readInt = read



-- addAll, merge elements of 2 list
-- @[[a]] -> list 1
-- @[[a]] -> list 2
-- @[[a]] -> merged list
addAll :: [[a]] -> [[a]] -> [[a]]
addAll _ [] = []
addAll [] _ = []
addAll (a:acc) (t:ts) = (a ++ t) : addAll acc ts



-- | 'readText', read the information file.
readText :: String -- ^ Text of the information file.
	-> (String -> a) -- ^ Function to convert the second line in a data containing information for the pattern
	-> (String, a) -- ^ (output file, pattern's information).
readText text reader = (read $ head linees, reader (linees !! 1))
	where
		linees = lines text



-- Helpers to create automatic list of students //////////////////////////////////////////////////////

-- | 'createList', Create a list a participants for test.
createList :: Int -- ^ Number of participants to be created
	-> String -- ^ List of participants.
createList n = show [ 'e' : show i | i <- [1..n]]



-- Functions to write .t2 ///////////////////////////////////////////////////////////////////

-- | 'writeT2', write the entire .t2 file.
writeT2 :: String -- ^ File path.
	-> [String] -- ^ List of notions names.
	-> NTree Cell -- ^ The tree.
	-> String -- ^ Pattern objects.
	-> IO()
writeT2 file notions tree objects = writeFile file $ show notions ++ "\n\nscript=" ++
	showTree tree ++ "\n\n" ++ objects ++ "\n\nteacherNotes = " ++ show (replicate (nbLeaf tree) "")



-- | 'writeT2Err', write the .t2 error file.
writeT2Err :: String -- ^ File path.
	-> [Possible a] -- ^ List of errors.
	-> IO()
writeT2Err file errs = writeFile file $ errors errs



-- | 'errors', convert errors in a .t2 format.
errors :: [Possible a] -- ^ Errors.
	-> String -- ^ Errors in .t2.
errors err = "Error=[" ++ errors' err True ++ "]\n"



-- 'errors'', cf 'errors'
errors' :: [Possible a] -- ^ Errors.
	-> Bool -- ^ If it's the first error.
	-> String -- ^ Errors in .t2.
errors' [] _ = ""
errors' (NotPossible msg:es) first = (if first then "\"" else ",\"") ++ msg ++ "\"" ++ errors' es False



-- Functions to read .t2 ///////////////////////////////////////////////////////////////////

-- | 'readHeader', return the first line of .t2 file.
readHeader :: String -- ^ .t2 content.
	-> String -- ^ First line of the .t2.
readHeader content = head $ lines content



-- | 'stripHeader', strip the header of .t2 content.
stripHeader :: String -- ^ .t2 content.
	-> String -- ^ .t2 content without the head (notions names + "script=").
stripHeader text = drop 7 $ unlines $ drop 2 $ lines text



-- 'stripFooter', return lines of the string without the footer.
stripFooter :: [String] -- ^ The file content in lines.
	-> [String] -- ^ All lines without footer's lines.
stripFooter [] = []
stripFooter (l:ls)
	| l == "" = []
	| otherwise = l : stripFooter ls



-- | 'readTree', read the tree from .t2.
readTree :: String -- ^ File path for .t2.
	-> IO (NTree Cell) -- ^ The tree stored in the .t2.
readTree file = do
	content <- readFile file
	return (read $ concat $ stripFooter $ lines $ stripHeader content)


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
splitList list (n:numbers) = first : splitList rest numbers
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
		diff1 = abs $ nbP - head list1 -- The difference between the preferred size and the size possible for an uniform repartion.
		diff2 = max (abs $ nbP - minimum list2) (abs $ nbP - maximum list2) -- The difference between the preferred size and the farthest size.



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
		decrease = if m<b && nb>2 then repartition' nbP n a b (m+1) else (False, [])



-- | 'repartitionUniform', repartition with the same size for each split, and with the closest possible size of the size wanted.
repartitionUniform :: Int -- ^ Number to split.
	-> Int -- ^ Size of a split wanted.
	-> Int -- ^ Above margin tolerated.
	-> Int -- ^ Below margin tolerated.
	-> Int -- ^ Variation.
	-> (Bool, [Int]) -- ^ If a repartition is possible then it returns (True, repartition) else (False, []).
repartitionUniform nbP n a b m
	| m<=a && mod nbP nb == 0 = (True, replicate (div nbP nb) nb) -- If the variation is still tolerated and we can split evenly.
	| m<=b && nb>1 && mod nbP nb1 == 0 = (True, replicate (div nbP nb1) nb1) -- If the variation is still tolerated and we can split evenly.
	| m<a || m<b = repartitionUniform nbP n a b (m+1) -- If the variation can increased, we continue.
	| otherwise = (False, []) -- No solution possible.
	where
		nb = n+m -- Size of a split (size wanted + positive variation).
		nb1 = n-m -- Size of a split (size wanted + negative variation).



-- 'repartition2Multiple', give a list of size depending of split information, for the best repartition. The list of plits can be done multiple of times.
repartition2Multiple :: Int -- ^ Number to split.
	-> [(Int, Int, Int)] -- ^ List of splits with for each split : the size wanted, the above margin and the below margin.
	-> Possible [[Int]] -- ^ Possible size repartition, list of list of splits.
repartition2Multiple nbP splits =
	case repartition nbP sumSize sumAbove sumBelow True of	-- If there is a repartition possible for the whole split, then there is a repartition possible for a list of splits.
		Possible splits' -> Possible $ map (\split -> possibleToList $ repartition2 split splits) splits' -- For each each big split, we make the subsplits.
		NotPossible _ -> NotPossible "Not possible to do a good repartition"
	where
		(sumSize, sumAbove, sumBelow) = sumTriplets splits	-- The big splits.



-- 'repartition2', give a list of size depending of split information, for the best repartition.
repartition2 :: Int -- ^ Number to split.
	-> [(Int, Int, Int)] -- ^ List of splits with for each split : the size wanted, the above margin and the below margin. Each split is done once.
	-> Possible [Int] -- ^ Possible size repartition.
repartition2 nbP splits
	| sumSize == nbP = Possible sizeSplits
	| (diffSum > 0 && diffSum <= sumAbove) || (diffSum < 0 && diffSum >= (-sumBelow)) = Possible $ repartition2' nbP diffSum splits
	| otherwise = NotPossible "repartition not possible"
	where
		sizeSplits = map (\(nb,_,_) -> nb) splits
		(sumSize, sumAbove, sumBelow) = sumTriplets splits
		diffSum = nbP - sumSize



-- | 'sumTriplets', sum a list of triplet.
-- > sumTriplet [(1,2,4),(-1,0,5)] == (0,2,9). 
sumTriplets :: Num a => [(a,a,a)] -- ^ Triplets.
	-> (a,a,a) -- ^ The sum.
sumTriplets [] = (0,0,0)
sumTriplets ((x,y,z):ts) = (x+x', y+y', z+z')
	where
		(x',y',z') = sumTriplets ts



-- | 'sumList', do the sum of 2 lists.
-- > sumList [1,2] [2,3,4] == [3,5,4]
sumList :: Num a => [a] -> [a] -> [a]
sumList [] [] = []
sumList (x:xs) [] = x : sumList xs []
sumList [] (y:ys) = y : sumList ys []
sumList (x:xs) (y:ys) = x + y : sumList xs ys




-- | 'repartition2'', cf 'repartition2', expect that 'repartition2'' is used only when there is a solution and the second parameter
-- take the difference between the number to split and the sum of sizes wanted
repartition2' :: Int -- ^ .
	-> Int -- ^ .
	-> [(Int, Int, Int)] -- ^ .
	-> [Int] -- ^ .
repartition2' _ _ [] = []
repartition2' nbP diff ((nbG,aG,bG):splits) = size : repartition2' (nbP-size) (diff-extra) splits
	where
		size = nbG + extra
		extra
			| diff > 0 && aG > 0 = min diff aG
			| diff < 0 && bG > 0 = max diff (-bG)
			| otherwise = 0