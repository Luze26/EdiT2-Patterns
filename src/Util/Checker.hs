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
match :: Identificator -> Cell -> Bool
match (Label l) (label,_,_,_) = l == label
match (Content c) (_,_,_,content) = c == (concat content)



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
-- @Items -> elems which must be found in the second list
-- @[String] -> list where we look
-- @Bool -> if elems of the first list appeared in the second list
contains :: Items -> [String] -> Bool
contains [] _ = True
contains ((Item x):xs) ys
	| elem x ys = contains xs ys
	| otherwise = False



-- containsNot, return True if no one of the elems of the first list are in the second list
-- @Items -> elems which shouldn't be found in the second list
-- @[String] -> list where we look
-- @Bool -> if elems of the first list don't appeared in the second list
containsNot :: Items -> [String] -> Bool
containsNot [] _ = True
containsNot ((Item x):xs) ys
	| elem x ys = False
	| otherwise = containsNot xs ys



-- replaceItemsValues, replace PropContent items by their value for the root node
-- @Cell -> the root node
-- @Items -> items
-- @Items -> items of the form Items String
replaceItemsValues :: Cell -> Items -> Items
replaceItemsValues _ [] = []
replaceItemsValues cell@(_, _, _, content) (PropContent:itemss) = Item (concat content) : replaceItemsValues cell itemss
replaceItemsValues cell (i:itemss) = i : replaceItemsValues cell itemss



-- checkUnder, return True if all the items in the first list are in all subtrees selected
-- @Items -> what to look for
-- @[NTree Cell] -> trees
checkUnder :: Items -> [NTree Cell] -> Bool
checkUnder _ [] = True
checkUnder itemss (t:trees)
	| contains items' (nodes [t]) == False = False
	| otherwise = checkUnder items' trees
	where
		items' = replaceItemsValues (node t) itemss



-- checkUnder', return True if all the items in the first list are in a subtree selected, or none of the item are in
-- @Items -> what to look for
-- @[NTree Cell] -> trees
checkUnder' :: Items -> [NTree Cell] -> Bool
checkUnder' _ [] = True
checkUnder' itemss (t:trees)
	| contains itemss nodes' || containsNot itemss nodes' = checkUnder' itemss trees
	| otherwise = False
	where
		nodes' = nodes [t]



-- checkBefore, check if the first item is before the second item (with left right traversal on root nodes of trees)
-- @Items -> what to look for
-- @[NTree Cell] -> trees
checkBefore :: Items -> [NTree Cell] -> Bool
checkBefore _ [] = False
checkBefore (_:[]) _ = False
checkBefore items@(a:b:_) (tree:trees)
	| b == comp = False
	| a == comp = True
	| otherwise = checkBefore items trees
	where
		comp = Item $ head $ cellComponents $ node tree