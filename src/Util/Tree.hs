module Tree
( 
	CellLabel,
	CellNumbering,
	CellFatherNumbering,
	CellComponents,
	Cell,
	NTree (..),
	showTree,
	readTree,
	node,
	subtrees,
	cellComponents,
	nbLeaf
) where

-- name of the column, in which the cell is
type CellLabel = String

-- 11011 (e.g)
type CellNumbering = String

-- 1101 (e.g)
type CellFatherNumbering = String

-- The component identification array
type CellComponents = [String]

type Cell = (CellLabel,CellNumbering,CellFatherNumbering,CellComponents)

data NTree a = Node a [NTree a] deriving (Eq,Ord,Show,Read)



-- node, return the cell
-- @NTree Cell -> the tree
-- @Cell -> the root's cell
node :: NTree Cell -> Cell
node (Node cell _) = cell



-- subtrees, return subtrees
-- @NTree Cell -> the tree
-- @[NTree Cell] -> sbtrees
subtrees :: NTree Cell -> [NTree Cell]
subtrees (Node _ strees) = strees



-- cellComponents, return the components
-- @Cell -> the cell
-- @CellComponents -> components
cellComponents :: Cell -> CellComponents
cellComponents (_,_,_,components) = components



-- nbLeaf, return the number of leaves for the given tree
-- @NTree Cell -> the tree
-- @Int -> number of leaves for the tree
nbLeaf :: NTree Cell -> Int
nbLeaf tree
	| sbtrees == [] = 1
	| otherwise = foldl (\acc x -> acc + nbLeaf x) 0 sbtrees
	where
		sbtrees = subtrees tree



-- readTree, parse a String into a NTree Cell
-- @String -> the string describing a tree
-- @NTree Cell -> the tree
readTree :: String -> NTree Cell
readTree tree = read $ stripHeader tree



-- stripHeader, return only the tree
-- @String -> the file content
-- @String -> the tree
stripHeader :: String -> String
stripHeader text = drop 7 $ concat $ drop 2 $ lines text



-- showTree, convert a tree in a string in .t2 format
-- @[String] -> column names
-- @NTree Cell -> the tree to convert
-- @String -> the tree in .t2 format
showTree :: [String] -> NTree Cell -> String
showTree columns tree = show columns ++ "\n\nscript=" ++ (showTree' True "" tree) 



-- showTree', convert a tree in string with correct indents
-- @Bool -> if there is another Ntree after him on the same level
-- @String -> tabulations
-- @NTree Cell -> the tree to convert
-- @String -> the tree in .t2 format
showTree' :: Bool -> String -> NTree Cell -> String
showTree' noComma tabs (Node cell sbtrees) = tabs ++ "Node " ++ (show cell) ++ starting ++ (showSubTrees ('\t':tabs) sbtrees) ++ ending
	where
		starting
			| sbtrees == [] = " ["
			| otherwise = " [\n"
		ending
			| noComma = "]"
			| otherwise = "],\n"



-- showSubTrees, call showTree for each subtrees
-- @String -> tabulations
-- @[NTree Cell] -> list of subtrees
-- @String -> subtrees in format .t2
showSubTrees :: String -> [NTree Cell] -> String
showSubTrees _ [] = ""
showSubTrees tabs (s:sbtrees) = showTree' noComma tabs s ++ (showSubTrees tabs sbtrees)
	where
		noComma = sbtrees == []