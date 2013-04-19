import Tree
import Constraints
import System.Environment( getArgs )


-- Checker, entry point
main :: IO()
main = do
	args <- getArgs
	start args


start :: [String] -> IO()
start (output:cstrs) = writeFile output $ show ((read (concat cstrs))::Cstr)

-- checkConstraint, check if the constaint is consistent with the tree
-- @NTree Cell -> the tree
-- @Cstr -> the constraint
-- @Bool -> if the tree respects the constraint
checkConstraint :: NTree Cell -> Cstr -> Bool
checkConstraint tree cstr
	| command cstr == "avec" = True
	| otherwise = False
