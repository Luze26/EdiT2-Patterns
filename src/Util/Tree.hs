module Tree
( 
	CellLabel,
	CellNumbering,
	CellFatherNumbering,
	CellComponents,
	Cell,
	NTree (..),
	showTree,
	readTree
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



-- readTree, parse a String into a NTree Cell
-- @String -> the string describing a tree
-- @NTree Cell -> the tree
readTree :: String -> NTree Cell
readTree tree = read tree



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
showTree' noComma tabs (Node cell subtrees) = tabs ++ "Node " ++ (show cell) ++ starting ++ (showSubTrees ('\t':tabs) subtrees) ++ ending
	where
		starting
			| subtrees == [] = " ["
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
showSubTrees tabs (s:subtrees) = showTree' noComma tabs s ++ (showSubTrees tabs subtrees)
	where
		noComma = subtrees == []
