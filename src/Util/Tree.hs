module Tree where

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



-- printTree, convert a tree in string with correct indents
-- @Bool -> if there is another Ntree after him on the same level
-- @String -> tabulations
-- @NTree Cell -> the tree to convert
-- @String -> the tree in .t2 format
printTree :: Bool -> String -> NTree Cell -> String
printTree noComma tabs (Node cell subtrees) = tabs ++ "Node " ++ (show cell) ++ starting ++ (printSubTrees ('\t':tabs) subtrees) ++ ending
	where
		starting
			| subtrees == [] = " ["
			| otherwise = " [\n"
		ending
			| noComma = "]"
			| otherwise = "],\n"



-- printSubTrees, call printTree for each subtrees
-- @String -> tabulations
-- @[NTree Cell] -> list of subtrees
-- @String -> subtrees in format .t2
printSubTrees :: String -> [NTree Cell] -> String
printSubTrees _ [] = ""
printSubTrees tabs (s:subtrees) = printTree noComma tabs s ++ (printSubTrees tabs subtrees)
	where
		noComma = subtrees == []
