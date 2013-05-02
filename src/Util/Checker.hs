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
		writeFile output $ show $ checkConstraint (readTree tree) ((read (concat cstrs))::Cstr)



-- checkConstraint, check if the constaint is consistent with the tree
-- @NTree Cell -> the tree
-- @Cstr -> the constraint
-- @Bool -> True if the tree respects the constraint, False otherwise
checkConstraint :: NTree Cell -> Cstr -> Bool
checkConstraint tree cstr
	| command cstr == "under" = -- checkUnder tree (wher cstr) (who cstr) 
	| otherwise = False



-- lookFor
lookFor :: NTree Cell -> Identificator -> [NTree Cell]
lookFor (node@(Node (label,_,_,_) subtrees)) id@(Label l)
	| label == l = node : foldl (\acc x -> acc ++ (lookFor x id)) [] subtrees
	| otherwise = foldl (\acc x -> acc ++ (lookFor x id)) [] subtrees

-- checkUnder
-- @String -> parent node
-- @[String] -> node contents which must all be under the same node
checkUnder :: NTree Cell -> String -> [String] -> Bool
checkUnder (Node (_,_,_,comp) subtree) root nodes
	| root == comps && subtree == [] = False
	| root == comps = contains nodes $ checkUnder'' subtree nodes
	| otherwise = checkUnder' subtree root nodes
	where comps = concat comp

checkUnder' :: [NTree Cell] -> String -> [String] -> Bool
checkUnder' [] _ _ = True
checkUnder' (t:trees) root nodes
	| checkUnder t root nodes = checkUnder' trees root nodes
	| otherwise = False

checkUnder'' :: [NTree Cell] -> [String] -> [String]
checkUnder'' [] _ = []
checkUnder'' ((Node (_,_,_,comp) subtrees):trees) nodes
	| elem comps nodes = comps : next
	| otherwise = next
	where
		comps = concat comp
		next = checkUnder'' subtrees nodes ++ (checkUnder'' trees nodes)

contains [] _ = True
contains (x:xs) ys
	| elem x ys = contains xs ys
	| otherwise = False
