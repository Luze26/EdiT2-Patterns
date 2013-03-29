import System.Environment( getArgs )
import Data.Char( intToDigit )
import Tree

main :: IO()
main = do
	args <- getArgs
	start (args ++ (createList 700))

start :: [String] -> IO()
start (file:m:args) = writeFile file ("[\"Topic\",\"Editor\",\"Section\",\"Editor\"]\n\nscript=" ++ (printTree True "" $ generate args $ readInt m))

generate :: [String] -> Int -> NTree Cell
generate n m = Node ("Root","1","null",[]) $ editorLevel (length n) n n m 0

editorLevel :: Int -> [String] -> [String] -> Int -> Int -> [NTree Cell]
editorLevel _ _ [] _ _ = []
editorLevel nb n (p:ps) m i = Node ("Topic", tId, "1", [ "Topic" ++ strI ]) [ Node ("Editor", eId, tId, [p]) $ sectionLevel eId strI (nb-1) n m 0 ] : editorLevel nb n ps m (i+1)
	where
		strI = show (i+1)
		tId = '1':strI
		eId = tId ++ "1"

sectionLevel :: String -> String -> Int -> [String] -> Int -> Int -> [NTree Cell]
sectionLevel _ _ _ _ 0 _ = []
sectionLevel eId tId nb n m i = Node ("Section", secId, eId, [ "Section" ++ tId ++ strI ]) [ Node ("Editor", e2Id, secId, [n !! e2I]) [] ] : sectionLevel eId tId nb n (m-1) (i+1)
	where
		strI = show (i+1)
		secId = eId ++ strI
		e2Id = secId ++ "1"
		e2I
			| mo >= (readInt tId)-1 = mo+1
			| otherwise = mo
			where 
				mo = mod i nb
		

readInt :: String -> Int
readInt = read

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


createList :: Int -> [String]
createList 1 = ["e1"]
createList n = ('e' : (show n)) : createList (n-1)
