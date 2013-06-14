-- | Module used for generate 'NTree Cell' from lists.
module Util.TreeGenerator
(
	Name,
	Level,
	generate
) where


import Util.Tree
import Util.Cell

-- | Name of a level (= name of a column = label name).
type Name = String

-- | 'Level', represent a level of the tree in a list way.
-- There is a list in the first level of the list, for each nodes on the level above.
-- And the list in the second level contains lists representing nodes for this Level.
type Level = (Name, [[[String]]])



-- | 'generate', generate a tree given the levels.
generate  :: [Level] -- ^ List of levels.
	-> NTree Cell -- ^ The tree built from the levels.
generate (l:lvls) = Node ("Root","1","null",[]) $ snd $ generateSubTree l lvls "1" 1



-- | 'generateSubTree', return a tuple with what it remains to treat for each level, and a list of subtrees.
generateSubTree :: Level -- ^ Level treated.
	-> [Level] -- ^ List of sublevels.
	-> String -- ^ Father's numbering.
	-> Int -- ^ Index of the node, in the subtree.
	-> ([Level], [NTree Cell]) -- ^ Tuple with what it remains to treat for each level, and a list of subtrees.
generateSubTree (name, []) lvls _ _ = ((name,[]):lvls, []) -- If there is no more subtree for this level, it's finish for this level.
generateSubTree (name, r:rows) lvls pid i -- We take the first list of the level => the first set of subtrees for the level (first = first of remaining ons)
	| null r = ((name,rows):lvls, [])	-- If there is no subtrees to be build, we return what it left and no subtrees.
	| otherwise = (ls, Node (name, id, pid, head r) subtree : nexttree) -- Otherwise, we construct the first subtree, and add him to the list of other
	where
		id = pid ++ show i	-- Node's numbering.
		(lsub, subtree)	-- lsub -> what we haven't treated yet. subtree -> subtrees for the given node.
			| null lvls = ([], [])
			| otherwise = generateSubTree (head lvls) (tail lvls) id 1
		(ls, nexttree) = generateSubTree (name, tail r : rows) lsub pid (i+1) -- ls -> what we haven't treated yet. nexttree -> next tree on the same level for the given node.