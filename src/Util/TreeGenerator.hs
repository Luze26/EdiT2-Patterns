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
generate lvls = Node ("Root","1","null",[]) $ generate' lvls "1" 1



-- generate', generate subtree given the levels
-- @[Level] -> levels
-- @String -> father numbering
-- @Int -> sub id
-- @[NTree Cell] -> subtrees
generate' :: [Level] -> String -> Int -> [NTree Cell]
generate' [] _ _ = []
generate' ((_, []:_):_) _ _ = []
generate' ((name, (r:row):rows):lvls) pid i =  (Node (name, id, pid, r) $ generate' lvls id 1) : generate' ((name, row:rows):lvls) pid (i+1) 
	where
		id = pid ++ (show i)
