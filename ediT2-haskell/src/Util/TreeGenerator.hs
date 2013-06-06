-- | Module used for generate 'NTree Cell' from lists.
module Util.TreeGenerator
(
	Name,
	Level,
	generate
) where

import Util.Tree

-- | Name of a level (= name of a column = label name).
type Name = String

-- | 'Level', represent a level of the tree in a list way.
-- There is a list in the first level of the list, for each nodes on the level above.
-- And the list in the second level contains lists representing nodes for this Level.
type Level = (Name, [[[String]]])



-- | 'generate', generate a tree given the levels.
generate  :: [Level] -- ^ List of levels.
	-> NTree Cell -- ^ The tree built from the levels.
generate (l:lvls) = Node ("Root","1","null",[]) $ generate' l lvls "1" 1



-- | 'generate'', generate a tree given the levels.
generate' :: Level -- ^ Level treated.
	-> [Level] -- ^ List of sublevels.
	-> String -- ^ Father's numbering.
	-> Int -- ^ Index of the node, in the subtree.
	-> [NTree Cell] -- ^ List of subtrees for the given level.
generate' (_, []) _ _ _ = []	-- If there is no more level, the generation is done for that branch.
generate' (name, [r]:rows) lvls pid i = Node (name, id, pid, r) subtree : generate' (name, rows) ls pid (i+1)	-- For each list of level's list, we create the corresponding node.
	where
		id = pid ++ show i	-- Node's numbering.
		(ls, subtree)			-- ls -> what we haven't treated yet. subtree -> subtrees for the given node.
			| null lvls = ([], [])
			| otherwise = generate'' (head lvls) (tail lvls) id 1



-- | 'generate''', return a tuple with what we haven't treated for each level, and a list of subtrees.
generate'' :: Level -- ^ Level treated.
	-> [Level] -- ^ List of sublevels.
	-> String -- ^ Father's numbering.
	-> Int -- ^ Index of the node, in the subtree.
	-> ([Level], [NTree Cell]) -- ^ Tuple with what we haven't treated for each level, and a list of subtrees.
generate'' (name, []) lvls _ _ = ((name,[]):lvls, [])
generate'' (name, r:rows) lvls pid i
	| null r = ((name,rows):lvls, [])
	| otherwise = (ls, Node (name, id, pid, head r) subtree : nexttree)
	where
		id = pid ++ show i	-- Node's numbering.
		(lsub, subtree)			-- lsub -> what we haven't treated yet. subtree -> subtrees for the given node.
			| null lvls = ([], [])
			| otherwise = generate'' (head lvls) (tail lvls) id 1
		(ls, nexttree) = generate'' (name, tail r :rows) lsub pid (i+1) -- ls -> what we haven't treated yet. nexttree -> next tree on the same level for the given node.