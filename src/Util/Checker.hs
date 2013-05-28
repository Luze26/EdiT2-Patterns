import Tree
import Constraints
import System.Environment( getArgs )
import Data.List( intersect )


-- Checker, entry point
main :: IO()
main = do
	args <- getArgs
	start args



start :: [String] -> IO()
start (output:input:cstrs) = do
		tree <- readFile input
		print $ show $ checkConstraint (readTree tree) ((read (concat cstrs))::Cstr)
		-- writeFile output $ show $ checkConstraint (readTree tree) ((read (concat cstrs))::Cstr)



-- checkConstraint, check if the constaint is consistent with the tree
-- @NTree Cell -> the tree
-- @Cstr -> the constraint
-- @Bool -> True if the tree respects the constraint, False otherwise
checkConstraint :: NTree Cell -> Cstr -> Bool
checkConstraint tree cstr
	| command cstr == "under" = checkUnder (items cstr) $ lookFor tree $ match (wher cstr)
	| command cstr == "!under" = not $ checkUnder (items cstr) $ lookFor tree $ match (wher cstr)
	| command cstr == "under?" = checkUnder' (items cstr) $ lookFor tree $ match (wher cstr)
	| command cstr == "!under?" = not $ checkUnder' (items cstr) $ lookFor tree $ match (wher cstr)
	| command cstr == "before" = checkBefore (items cstr) $ lookFor tree $ match (wher cstr)
	| otherwise = False



-- match, return True if a cell match the identificator
-- @Identificator -> identificator
-- @Cell -> the cell
-- @Bool -> True if the cell match the identificator
match :: Identificator -> NTree Cell -> Bool
match (Label l) (Node (label,_,_,_) _) = l == label
match (Content c) (Node (_,_,_,content) _) = c == (concat content)
match (Identificator l c) (Node (label,_,_,content) _) = c == (concat content) && l == label



-- lookFor, return trees with a root node included in the identificator
-- @NTree Cell -> the tree
-- @Identificator -> identificator used to know where we must look
-- @[NTree Cell] -> subtrees involved
lookFor :: NTree Cell -> (NTree Cell -> Bool) -> [NTree Cell]
lookFor node@(Node _ sbtrees) match'
	| match' node  = node : foldl (\acc x -> acc ++ (lookFor x match')) [] sbtrees
	| otherwise = foldl (\acc x -> acc ++ (lookFor x match')) [] sbtrees



-- notContainedId, return the list of identifcatos not found in the tree
-- @NTree Cell -> the tree
-- @Identificatos -> identificators
notContainedId :: Identificators -> NTree Cell -> Identificators
notContainedId ids tree
	| idLeft == [] = []
	| otherwise = foldl (intersect) idLeft (map (notContainedId idLeft) (subtrees tree))
	where
		idLeft = notContainedId' tree ids



notContainedId' :: NTree Cell -> Identificators -> Identificators
notContainedId' _ [] = []
notContainedId' tree (id:ids)
	| match id tree = notContainedId' tree ids
	| otherwise = id :  notContainedId' tree ids



-- checkUnder, return True if all the items in the first list are in all subtrees selected
-- @Identificatos -> what to look for
-- @[NTree Cell] -> trees
checkUnder :: Identificators -> [NTree Cell] -> Bool
checkUnder _ [] = True
checkUnder itemss (t:trees)
	| notContainedId itemss t == [] = checkUnder itemss trees
	| otherwise = False



-- checkUnder', return True if all the items in the first list are in a subtree selected, or none of the item are in
-- @Identificatos -> what to look for
-- @[NTree Cell] -> trees
checkUnder' :: Identificators -> [NTree Cell] -> Bool
checkUnder' _ [] = True
checkUnder' itemss (t:trees)
	| badId == itemss || badId == [] = checkUnder' itemss trees
	| otherwise = False
	where
		badId = notContainedId itemss t


-- checkBefore, check if the first item is before the second item (with left right traversal on root nodes of trees)
-- @Identificatos -> what to look for
-- @[NTree Cell] -> trees
checkBefore :: Identificators -> [NTree Cell] -> Bool
checkBefore _ [] = False
checkBefore (_:[]) _ = False
checkBefore items@(a:b:_) (tree:trees)
	| match a tree = True
	| match b tree = False
	| otherwise = checkBefore items trees