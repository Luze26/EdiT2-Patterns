import RT_Constraints( check )
import Tree.hs ( parse )
import System.Environment( getArgs )

{-===========================================================================-}
{-============================== MAIN FUNCTION ==============================-}
main :: IO()
main = do
	args <- getArgs
	writeFile (head args) $ check $ parse $ concat $ tail args
