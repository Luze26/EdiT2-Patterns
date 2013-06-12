-- | Module to represent a node of a tree for ediT2.
module Util.Cell where

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
mergeCell [] = ("","","",[])
mergeCell ((x,y,z,contents):cells) = (x,y,z, foldl (++) contents $ map (cellComponents) cells)



-- | 'eqCellsContent', equality function based only on the contents.
eqCellsContent :: Cell -- ^ First cell.
	-> Cell -- ^ Second cell.
	-> Bool -- ^ 'True' if the two cells have the same content.
eqCellsContent a b = length cA == length cB && and (map (`elem` cB) cA)
	where
		cA = cellComponents a
		cB = cellComponents b