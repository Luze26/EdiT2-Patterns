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



-- parse, parse a String into a NTree
-- @String -> the string describing a tree
-- @NTree Cell -> the tree
parse :: String -> NTree Cell
parse tree = read tree::NTree Cell


printTree :: Bool -> String -> NTree Cell -> String
printTree noComma tabs (Node cell subtrees) = tabs ++ "Node " ++ (show cell) ++ starting ++ (printSubTrees ('\t':tabs) subtrees) ++ ending
	where
		starting
			| subtrees == [] = " ["
			| otherwise = " [\n"
		ending
			| noComma = "]"
			| otherwise = "],\n"

printSubTrees :: String -> [NTree Cell] -> String
printSubTrees _ [] = ""
printSubTrees tabs (s:subtrees) = printTree noComma tabs s ++ (printSubTrees tabs subtrees)
	where
		noComma = subtrees == []
