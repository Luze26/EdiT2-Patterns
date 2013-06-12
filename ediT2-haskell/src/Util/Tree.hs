-- | Module containing the data structure for the tree and useful functions on tree.
module Util.Tree
( 
	CellLabel,
	CellNumbering,
	CellFatherNumbering,
	CellComponents,
	Cell,
	NTree (..),
	showTree,
	node,
	subtrees,
	cellComponents,
	nbLeaf
) where


-- ///////////////////////////////////// Cell Module /////////////////////////////////////////////

-- | Name of the column (notion), in which the cell is.
type CellLabel = String

-- | The number of the cell (differents cells can have the same number across the tree).
type CellNumbering = String

-- | The number of the father, 'null' for the root node.
type CellFatherNumbering = String

-- | The component identification array.
type CellComponents = [String]

-- | 'Cell' represents a node
type Cell = (CellLabel,CellNumbering,CellFatherNumbering,CellComponents)



-- | 'cellComponents', return the components of a cell.
cellComponents :: Cell -- ^ The cell.
	-> CellComponents -- ^ components of the cell.
cellComponents (_,_,_,components) = components



-- | 'mergeCell', merge cells
mergeCell :: [Cell] -- ^ cells to merge
	-> Cell -- ^ cells merged
mergeCell [] = ("","","","",[])
mergeCell ((x,y,z,contents):cells) = (x,y,z, foldl (++) contents $ map (cellComponents) cells)



-- ///////////////////////////////////// Tree Module /////////////////////////////////////////////

data NTree a =
	-- | Data structure representing a tree. 'Node a' for the node and '[NTree a]' for the subtrees
	Node a [NTree a] deriving (Eq,Ord,Show,Read)



-- | 'node', return the cell of a 'NTree Cell'.
node :: NTree a -- ^ The tree.
	-> a -- ^ The root's cell of the tree.
node (Node cell _) = cell



-- | 'subtrees', return subtrees of a NTree.
subtrees :: NTree a -- ^ The tree.
	-> [NTree a] -- ^ Subtrees of the tree.
subtrees (Node _ strees) = strees



-- | 'replaceSubtrees', replace subtrees of a NTree.
subtrees :: NTree a -- ^ The tree.
	-> [NTree a] -- ^ New subtrees.
	-> [NTree a] -- ^ Subtrees of the tree.
subtrees (Node cell _) sbtrees = Node cell sbtrees



-- | 'nbLeaf', return the number of leaves for the given tree
nbLeaf :: (Eq a) => NTree a -- ^ The tree.
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
mergeTrees sbtrees merger = Node (merger (map (node) sbtrees)) (subtrees $ head sbtrees)



-- | 'normalize', normalize a tree, merge nodes having the same parent and an identical subtree.
normalize :: NTree a -- ^ The tree.
	-> (a -> a -> Bool) -- ^ Eq fonction.
	-> ([a] -> a) -- ^ Merger.
	-> NTree a	-- ^ Normalized tree.
normalize tree eq merger = replaceSubtrees tree $ normalize' (subtrees tree) eq merger



-- | 'normalize'', cf 'normalize'
normalize' :: [NTree a] -- ^ Subtrees on the same level.
	-> (a -> a -> Bool) -- ^ Eq fonction.
	-> ([a] -> a) -- ^ Merger.
	-> [NTree a] -- ^ Normalized subtrees.
normalize' [] _ = []
normalize' trees@(t:ts) eq merger
	| null $ subtrees t = mergeTrees trees merger
	| otherwise = mergeTrees same : normalize' different eq merger
	where
		(same, different) = sameSubtrees t ts eq



sameSubtrees :: NTree a -- ^ Subtrees of reference.
	->  [NTree a] -- ^ Subtrees on the same level.
	-> (a -> a -> Bool) -- ^ Eq fonction.
	-> ([NTree a], [NTree a])	-- ^ (Trees having an identical subtrees, a different Subtrees) from the reference subtree.
sameSubtrees _ [] _ = ([],[])
sameSubtrees t trees eq = 