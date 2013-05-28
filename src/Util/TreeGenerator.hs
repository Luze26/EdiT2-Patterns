module TreeGenerator
(
	Name,
	Level,
	generate
) where

import Tree

type Name = String
type Level = (Name, [[[String]]])



-- generate, generate a tree given the levels
-- @[Level] -> list of levels
-- @NTree Cell -> the tree
generate  :: [Level] -> NTree Cell
generate (l:lvls) = Node ("Root","1","null",[]) $ generate' l lvls "1" 1



generate' :: Level -> [Level] -> String -> Int -> [NTree Cell]
generate' (_, []) _ _ _ = []
generate' (name, [r]:rows) lvls pid i = (Node (name, id, pid, r) subtree) : generate' (name, rows) ls pid (i+1)
	where
		id = pid ++ (show i)
		(ls, subtree)
			| lvls == [] = ([], [])
			| otherwise = generate'' (head lvls) (tail lvls) id 1



generate'' :: Level -> [Level] -> String -> Int -> ([Level], [NTree Cell])
generate'' (name, []:rows) lvls _ _ = ((name,rows):lvls, [])
generate'' (name, []) lvls _ _ = ((name,[]):lvls, [])
generate'' (name, (r:row):rows) lvls pid i = (ls, (Node (name, id, pid, r) subtree):nexttree)
	where
		id = pid ++ (show i)
		(lsub, subtree)
			| lvls == [] = ([], [])
			| otherwise = generate'' (head lvls) (tail lvls) id 1
		(ls, nexttree) = generate'' (name, row:rows) lsub pid (i+1)
