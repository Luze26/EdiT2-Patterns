import Tree
import Constraints
import System.Environment( getArgs )


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
	| command cstr == "under" = checkUnder (who cstr) $ lookFor tree $ match (wher cstr)
	| command cstr == "under?" = checkUnder' (who cstr) $ lookFor tree $ match (wher cstr)
	| otherwise = False



-- match
match :: Identificator -> Cell -> Bool
match (Label l) (label,_,_,_) = l == label



-- lookFor, return trees with a root node included in the identificator
-- @NTree Cell -> the tree
-- @Identificator -> identificator used to know where we must look
-- @[NTree Cell] -> subtrees involved
lookFor :: NTree Cell -> (Cell -> Bool) -> [NTree Cell]
lookFor node@(Node cell subtrees) match'
	| match' cell  = node : foldl (\acc x -> acc ++ (lookFor x match')) [] subtrees
	| otherwise = foldl (\acc x -> acc ++ (lookFor x match')) [] subtrees



-- nodes, return a list of nodes content under the given trees
-- @[NTree Cell] -> trees
-- @[String] -> contents of the nodes
nodes :: [NTree Cell] -> [String]
nodes [] = []
nodes ((Node (_,_,_,contents) subtrees):trees)  = (contents' : nodes subtrees) ++ nodes trees
	where
		contents' = concat contents



-- contains, return True if all the elems of the first list are in the second list
-- @[String] -> elems which must be found in the second list
-- @[String] -> list where we look
-- @Bool -> if elems of the first list appeared in the second list
contains :: [String] -> [String] -> Bool
contains [] _ = True
contains (x:xs) ys
	| elem x ys = contains xs ys
	| otherwise = False



-- containsNot, return True if no one of the elems of the first list are in the second list
-- @[String] -> elems which shouldn't be found in the second list
-- @[String] -> list where we look
-- @Bool -> if elems of the first list don't appeared in the second list
containsNot :: [String] -> [String] -> Bool
containsNot [] _ = True
containsNot (x:xs) ys
	| elem x ys = False
	| otherwise = containsNot xs ys



-- checkUnder, return True if all the items in the first list are in all subtrees selected
-- @[String] -> what to look for
-- @[NTree Cell] -> trees
checkUnder :: [String] -> [NTree Cell] -> Bool
checkUnder _ [] = True
checkUnder items (t:trees)
	| contains items (nodes [t]) == False = False
	| otherwise = checkUnder items trees



-- checkUnder', return True if all the items in the first list are in a subtree selected, or none of the item are in
-- @[String] -> what to look for
-- @[NTree Cell] -> trees
checkUnder' :: [String] -> [NTree Cell] -> Bool
checkUnder' _ [] = True
checkUnder' items (t:trees)
	| contains items nodes' || containsNot items nodes' = checkUnder' items trees
	| otherwise = False
	where
		nodes' = nodes [t]
