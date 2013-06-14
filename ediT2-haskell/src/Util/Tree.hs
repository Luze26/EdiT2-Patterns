-- | Module containing the data structure for the tree and useful functions on tree.
module Util.Tree
( 
	NTree (..),
	showTree,
	node,
	subtrees,
	nbLeaf,
	normalize,
	lookFor
) where


data NTree a
	-- | Data structure representing a tree.
	= Node a [NTree a] -- ^ 'Node a' for the node and '[NTree a]' for the subtrees.
	| EmptyTree -- ^ An empty tree.
	deriving (Eq,Ord,Show,Read)



-- | 'node', return the cell of a 'NTree Cell'.
node :: NTree a -- ^ The tree.
	-> a -- ^ The root's cell of the tree.
node (Node cell _) = cell



-- | 'subtrees', return subtrees of a NTree.
subtrees :: NTree a -- ^ The tree.
	-> [NTree a] -- ^ Subtrees of the tree.
subtrees (Node _ strees) = strees



-- | 'replaceSubtrees', replace subtrees of a NTree.
replaceSubtrees :: NTree a -- ^ The tree.
	-> [NTree a] -- ^ New subtrees.
	-> NTree a -- ^ New tree.
replaceSubtrees (Node cell _) = Node cell



-- | 'nbLeaf', return the number of leaves for the given tree
nbLeaf :: NTree a -- ^ The tree.
	-> Int -- ^ Number of leaves for the tree.
nbLeaf tree
	| null sbtrees = 1
	| otherwise = foldl (\acc x -> acc + nbLeaf x) 0 sbtrees
	where
		sbtrees = subtrees tree



-- | 'showTree', convert a tree in a string in .t2 format. 
showTree :: (Show a) => NTree a -- ^ The tree to convert.
	-> String -- ^ The tree in .t2 format.
showTree = showTree' True ""



-- 'showTree'', cf 'showTree'.
showTree' :: (Show a) => Bool -- ^ If there is another tree after him on the same level (if it's not the last subtree of the level).
	-> String -- ^ Tabulations.
	-> NTree a -- ^ The tree to convert in .t2.
	-> String -- ^ The tree in .t2 format.
showTree' noComma tabs (Node cell sbtrees) = tabs ++ "Node " ++ show cell ++ starting ++ ending
	where
		starting
			| null sbtrees = " ["		-- If there is no subtrees.
			| otherwise = " [\n" ++ showSubTrees ('\t':tabs) sbtrees	-- Otherwise, we translate each subtree in .t2 with the correct indentation.
		ending
			| noComma = "]"		-- If it's the last subtree of the level, there is no comma.
			| otherwise = "],\n"




-- | 'showSubTrees', call showTree for each subtrees.
showSubTrees :: (Show a) => String -- ^ Tabulations.
	-> [NTree a] -- ^ List of subtrees.
	-> String -- ^ Subtrees in format .t2.
showSubTrees _ [] = ""
showSubTrees tabs (s:sbtrees) = showTree' noComma tabs s ++ showSubTrees tabs sbtrees
	where
		noComma = null sbtrees


-- | 'mergeTrees', merge content of nodes, keeping the subtree of the first node.
mergeTrees :: [NTree a] -- ^ Subtrees to merge
	-> ([a] -> a) -- ^ Merged root nodes
	-> NTree a -- ^ Merged subtrees.
mergeTrees sbtrees merger = Node (merger (map node sbtrees)) (subtrees $ head sbtrees)



-- | 'normalize', normalize a tree, merge nodes having the same parent and an identical subtree.
normalize :: NTree a -- ^ The tree.
	-> (a -> a -> Bool) -- ^ Eq fonction.
	-> ([a] -> a) -- ^ Merger.
	-> NTree a	-- ^ Normalized tree.
normalize tree eq merger = replaceSubtrees tree $ normalize' eq merger (subtrees tree)



-- | 'normalize'', cf 'normalize'
normalize' :: (a -> a -> Bool) -- ^ Eq function.
	-> ([a] -> a) -- ^ Merger.
	-> [NTree a] -- ^ Subtrees on the same level.
	-> [NTree a] -- ^ Normalized subtrees.
normalize' _ _ [] = []
normalize' eq merger trees@(t:ts)
	| null $ subtrees t = [mergeTrees trees merger]
	| otherwise = mergeTrees (x:same) merger : normalize' eq merger different
	where
		(x:xs) = map (\tree -> replaceSubtrees tree (normalize' eq merger (subtrees tree))) trees
		(same, different) = sameSubtrees x xs eq



-- | 'sameSubtrees', return a list of trees having the same subtree (similarity defined by the Eq function), and another list with differnts trees.
sameSubtrees :: NTree a -- ^ Subtrees of reference, included in the first list.
	->  [NTree a] -- ^ Subtrees on the same level.
	-> (a -> a -> Bool) -- ^ Eq function.
	-> ([NTree a], [NTree a]) -- ^ (Trees having an identical subtrees, a different Subtrees) from the reference subtree.
sameSubtrees _ [] _ = ([],[])
sameSubtrees t (tree:trees) eq
	| length sbt == length sbtree && nodesExists eq (map node sbt) (map node sbtree) = (tree:s, d)
	| otherwise = (s, tree:d)
	where
		sbt =  subtrees t
		sbtree = subtrees tree
		(s,d) = sameSubtrees t trees eq



nodesExists :: (a -> a -> Bool) -- ^ Eq function.
	-> [a]
	-> [a]
	-> Bool
nodesExists _ [] _ = True
nodesExists eq (n:ns) nodes
	| nodesExists' eq n nodes = nodesExists eq ns nodes
	| otherwise = False



nodesExists' :: (a -> a -> Bool) -- ^ Eq function.
	-> a
	-> [a]
	-> Bool
nodesExists' _ _ [] = False
nodesExists' eq node (n:ns)
	| eq node n = True
	| otherwise = nodesExists' eq node ns



-- | 'lookFor', return trees node matched by the function.
lookFor :: NTree a -- ^ The tree.
	-> (NTree a -> Bool) -- ^ Function determining if we must keep a subtree.
	-> [NTree a] -- ^ Subtrees matched by the function.
lookFor nodee@(Node _ sbtrees) match'
	| match' nodee  = nodee : foldl (\acc x -> acc ++ lookFor x match') [] sbtrees
	| otherwise = foldl (\acc x -> acc ++ lookFor x match') [] sbtrees