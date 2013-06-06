import qualified Constraint.Checker as Constraint
import qualified Jigsaw.Jigsaw as Jigsaw
import qualified RT.RT as RT
import qualified Simulation.Simulation as Simulation
import System.Environment( getArgs )



-- | 'main', entry point. Expect a file path in argument pointing to a file containing information needed.
main :: IO()
main = do
	args <- getArgs
	if (length args < 1) 
	then do putStrLn "Not enough arguments.\nUsage: ediT2-haskell <pattern's name> [arguments]"
	else do main' args



main' :: [String] -> IO ()
main' (pattern:args)
	| pattern == "Constraint" = Constraint.run args
	| pattern == "Jigsaw" = Jigsaw.run args
	| pattern == "RT" = RT.run args
	| pattern == "Simulation" = Simulation.run args
	| otherwise = putStrLn "Unknow pattern or command. Possibilities: RT, Jigsaw, Simulation, Constraint"