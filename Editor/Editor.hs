import System.Environment( getArgs )
import Data.Char( intToDigit )
import Tree

main :: IO()
main = do
	args <- getArgs
	start (args ++ [createList 700])

start :: [String] -> IO()
start (file:m:args) = writeFile file $ generate ((read $ concat $ args) :: [String]) $ readInt m

generate :: [String] -> Int -> String
generate n m = show (Node ("Root","1","null",[]) $ editorLevel n m 0 :: NTree Cell)

editorLevel :: [String] -> Int -> Int -> [NTree Cell]
editorLevel n m i
	| i < length n = Node ("Topic", tId, "1", [ "Topic" ++ strI ]) [ Node ("Editor", eId, tId, [n !! i]) $ sectionLevel eId strI n m 0 ] : editorLevel n m (i+1)
	| otherwise = []
	where
		strI = show (i+1)
		tId = '1':strI
		eId = tId ++ "1"

sectionLevel :: String -> String -> [String] -> Int -> Int -> [NTree Cell]
sectionLevel _ _ _ 0 _ = []
sectionLevel eId tId n m i = Node ("Section", secId, eId, [ "Section" ++ tId ++ strI ]) [ Node ("Editor", e2Id, secId, [n !! e2I]) [] ] : sectionLevel eId tId n (m-1) (i+1)
	where
		strI = show (i+1)
		secId = eId ++ strI
		e2Id = secId ++ "1"
		e2I
			| mo >= (readInt tId)-1 = mo+1
			| otherwise = mo
			where 
				mo = mod i (length n-1)
		

readInt :: String -> Int
readInt = read


createList n = '[':(cl n) ++ "]"

cl :: Int -> String
cl 1 = "\"e1\""
cl n = '\"':'e' : (show n) ++ "\"," ++ cl (n-1)
