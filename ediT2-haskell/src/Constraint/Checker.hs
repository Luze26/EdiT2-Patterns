-- | Module to verify constraints on .t2 file.
module Constraint.Checker
(
	run,
	check,
	checkConstraint
) where


import Util.Tree
import Util.Cell
import Util.T2
import Util.Util ( readT2, readConstraints )
import Util.Constraints
import Data.List( intersect, (\\) )
import qualified Jigsaw.JigsawConstraints as Jigsaw



-- | 'run', entry point. Expect a file path for the output, the .t2 input file and a file containing constraints to check.
run :: [String] -> IO()
run (output:input:cstrs:_) = do
		t2 <- readT2 input
		constraints <- readConstraints cstrs
		writeFile output $ check t2 constraints
run _ = putStrLn "Not enough arguments for constraint.\nUsage: ediT2-haskell Constraint <output file> <.t2 file> <constraints file>"



-- | 'check', check all the constraints.
check :: T2 -- ^ The t2 file.
	-> [Cstr] -- ^ Constraints to check.
	-> String -- ^ Results.
check _ [] = "" -- If there is no constraints.
check t2 (c:cstrs) = show  (checkConstraint t2 c) ++ "\n" ++ check t2 cstrs 



-- | 'checkConstraint', check if the constraint is consistent with the tree.
checkConstraint :: T2 -- ^ The t2 file.
	-> Cstr -- ^ The constraint.
	-> (Bool, [String]) -- ^ 'True', if the tree respects the constraint, 'False' otherwise. And [String] for the errors.
checkConstraint t2 cstr =
	case cstr of
		CstrPattern pattern constraint -> 
			case pattern of
				"Jigsaw" -> case Jigsaw.check constraint t2 of
					CstrBis c -> checkConstraint t2 c
					Result (ok, err) -> (ok, err)
				_ -> (False, ["Unknow pattern: " ++ pattern])
		Cstr { items = iteems, command = cmd, wher = wheer } ->
			case cmd of
				"under" -> let (ok, ids) = checkUnder iteems $ lookFor tree $ match wheer in (ok, identificatorsToString ids)
				"!under" -> let (ok, ids) = checkUnder iteems $ lookFor tree $ match wheer in (not ok, identificatorsToString $ iteems \\ ids)
				"under?" -> (checkUnderOrNothing iteems $ lookFor tree $ match wheer, [])
				"!under?" -> (not $ checkUnderOrNothing iteems $ lookFor tree $ match wheer, [])
				"under1" -> (checkUnder1 iteems $ lookFor tree $ match wheer, [])
				"before" -> checkBefore iteems $ lookFor tree $ match wheer
				_ -> (False, ["Unknow command: " ++ cmd])
	where
		tree = t2Tree t2



-- | 'match', return True if the root cell matchs the identificator.
match :: Identificator -- ^ The identificator.
	-> NTree Cell -- ^ The tree.
	-> Bool -- ^ 'True', if the cell at the root of the tree matchs the identifcator. 'False', otherwise.
match (Label l) (Node (label,_,_,_) _) = l == label
match (Content c) (Node (_,_,_,content) _) = c `elem` content
match (Identificator l c) (Node (label,_,_,content) _) = c `elem` content && l == label



-- | 'checkUnder', return 'True' if all the items in the first list are in all subtrees selected.
checkUnder :: Identificators -- ^ Identificators, matching items that must be in all subtrees.
	-> [NTree Cell] -- ^ Subtrees.
	-> (Bool, Identificators) -- ^ ('True', if all the items matched by the identificators are in all the subtrees, faulty identificators).
checkUnder _ [] = (True, [])
checkUnder itemss (t:trees)
	| null faulty = checkUnder itemss trees
 | otherwise = let (_, faulty2) = checkUnder itemss trees in (False, removeDuplicate (faulty ++ faulty2))
	where
		faulty = notContainedId itemss t



-- | 'checkUnderOrNothing', return True if all the items in the first list are in a subtree selected, or none of the items are in.
checkUnderOrNothing :: Identificators -- ^ Identificators, matching items.
	-> [NTree Cell] -- ^ Subtrees.
	-> Bool -- ^ 'True', if all the items matched by the identificators are in a subtree or none of them. 'False', otherwise.
checkUnderOrNothing _ [] = True
checkUnderOrNothing itemss (t:trees)
	| badId == itemss || null badId = checkUnderOrNothing itemss trees
	| otherwise = False
	where
		badId = notContainedId itemss t



-- | 'checkUnder1', return 'True' if at least one items in the first list is in the subtree (for each subtrees).
checkUnder1 :: Identificators -- ^ Identificators, matching items.
	-> [NTree Cell] -- ^ Subtrees.
	-> Bool -- ^ 'True' if at least one items in the first list is in the subtree (for each subtrees). 'False', otherwise.
checkUnder1 _ [] = True
checkUnder1 itemss (t:trees)
	| containsId itemss itemss 1 t = checkUnder1 itemss trees
	| otherwise = False



-- | 'checkNotUnder'', return 'True' if only one item in the first list, is in a subtree selected, or none of the items are in.
checkNotUnder' :: Identificators -- ^ Identificators, matching items.
	-> [NTree Cell] -- ^ Subtrees.
	-> Bool -- ^ 'True' if only one items in the first list is in the subtree (for each subtrees). 'False', otherwise.
checkNotUnder' _ [] = True
checkNotUnder' itemss (t:trees)
	| badId == 1 || badId == 0 = checkNotUnder' itemss trees
	| otherwise = False
	where
		badId = length $ notContainedId itemss t



-- | 'checkBefore', check if the first item is before the second item (with left right traversal on root nodes of trees).
checkBefore :: Identificators -- ^ Identificators.
	-> [NTree Cell] -- ^ Subtrees.
	-> (Bool, [String]) -- 'True', if the first item is before the second item. 'False', otherwise.
checkBefore [] [] = (True, [])
checkBefore ids [] = (False, ["Not found: " ++ show (identificatorsToString ids)])
checkBefore (_:[]) _ = (True, [])
checkBefore items@(a:b:xs) ts@(tree:trees)
	| match a tree && not matchB = checkBefore (b:xs) ts
	| matchB = (False, [identificatorToString b ++ " is before or on the same place than " ++ identificatorToString a])
	| otherwise = checkBefore items trees
	where
		matchB = match b tree



-- ////////////////////////////////// Util functions

-- | 'removeDuplicate', remove duplicated elems of a list.
removeDuplicate :: (Eq a) => [a] -> [a]
removeDuplicate [] = []
removeDuplicate (x:xs)
	| x `elem` xs = removeDuplicate xs
	| otherwise = x : removeDuplicate xs



-- | 'notContainedId', return the list of identifcatos not matched in the tree.
notContainedId :: Identificators -- ^ Identificators.
	-> NTree Cell -- ^ The tree.
	-> Identificators -- ^ Identificators not matched.
notContainedId ids tree
	| null idRemaining = [] -- If there is no more identificators not matched, we return an empty list.
	| otherwise = foldl intersect idRemaining (map (notContainedId idRemaining) (subtrees tree)) -- Return the list of identificators not matched by the tree and its subtrees.
	where
		idRemaining = notContainedId' tree ids -- Identificators not matched by the root node of the tree.



-- | 'notContainedId'', return the list of identifcatos not matched for the root node of the tree.
notContainedId' :: NTree Cell -- ^ The tree.
	-> Identificators -- ^ Identificators.
	-> Identificators -- ^ Identificators not matched.
notContainedId' _ [] = []
notContainedId' tree (id:ids)
	| match id tree = notContainedId' tree ids
	| otherwise = id :  notContainedId' tree ids



-- | 'containsId', check if a given number of identificators are matched.
containsId :: Identificators -- ^ Identificators.
	-> Identificators -- ^ Identificators remaining to check for the root node.
	-> Int -- ^ Number of matchs wanted.
	-> NTree Cell -- ^ The tree.
	-> Bool -- ^ 'True', if there is a number of matched wanted. 'False', otherwise.
containsId _ _ 0 _ = True
containsId [] _ _ _ = False
containsId ids [] x tree =  containsId' ids x $ subtrees tree
containsId ids (i:is) x tree
	| match i tree = containsId ids is (x-1) tree
	| otherwise = containsId ids is x tree



-- | 'containsId'', check if a given number of identificators are matched in at least one of the subtrees.
containsId' :: Identificators -- ^ Identificators.
	-> Int -- ^ Number of match.
	-> [NTree Cell] -- ^ Subtrees.
	-> Bool
containsId' _ _ [] = False
containsId' ids x (t:tree)
	| containsId ids ids x t = True
	| otherwise = containsId' ids x tree