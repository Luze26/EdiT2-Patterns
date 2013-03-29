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

data NTree a = Node a [NTree a] deriving (Eq,Ord,Show)



-- parse, parse a String into a NTree
-- @String -> the string describing a tree
-- @NTree Cell -> the tree
parse :: String -> NTree Cell
parse tree = let (cell, string) = parseCell tree in Node cell []



-- parseCell, parse the first cell found in the string
-- @String, the string
-- @(Cell, String) -> the cell parsed, the string remaining
parseCell :: String -> (Cell, String)
parseCell string = ((label, fatherNumbering, numbering, read components) ,xs3) 
	where
		(label, xs) = takeBetween string '"'
		(fatherNumbering, xs1) = takeBetween xs '"'
		(numbering, xs2) = takeBetween xs1 '"'
		(components, xs3) = takeNextList xs2 False []
		

-- takeBetween, return a tuple with the string between the next 2 characters equals to the second parameter
-- @String -> the string to anlyse
-- @Char -> delimiter
-- @(String, String) -> (string between the two delimiters, remaining string)
takeBetween :: String -> Char -> (String, String)
takeBetween string c = takeBetween' string c False []



-- takeBetween', return a tuple with the string between the next 2 characters equals to the second parameter
-- @String -> the string to anlyse
-- @Char -> delimiter
-- @Bool -> if we are between the two delimiters
-- @String -> the string between the two delimiters
-- @(String, String) -> (string between the two delimiters, remaining string)
takeBetween' :: String -> Char -> Bool -> String -> (String, String) 
takeBetween' [] _ _ _ = ([],[])
takeBetween' (x:xs) c b del
	| c == x && b = (reverse del,xs)
	| c == x && not b = takeBetween' xs c True del
	| b = takeBetween' xs c b (x:del)
	| otherwise = takeBetween' xs c b del



-- takeNextList, return a tuble with a string containing the next list, and the remaining string
-- @String -> the string to anlyse
-- @Bool -> if we are on the list
-- @String -> the string representing the list
-- @(String, String) -> (string of the next list in the string, remaining string)
takeNextList :: String -> Bool -> String -> (String, String) 
takeNextList [] _ _ = ([],[])
takeNextList (x:xs) b del
	| x == ']' && b = (reverse (x:del),xs)
	| x == '[' && not b = takeNextList xs True (x:del)
	| b = takeNextList xs b (x:del)
	| otherwise = takeNextList xs b del
