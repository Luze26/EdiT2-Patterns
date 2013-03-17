import RT_Generate( generate )
import Util
import RT_Info( readInfo )
import System.Environment( getArgs )

{-===========================================================================-}
{-============================== MAIN FUNCTION ==============================-}
main :: IO()
main = do
	args <- getArgs
	writeFile (head args) $ generate $ readInfo $ concat $ tail args