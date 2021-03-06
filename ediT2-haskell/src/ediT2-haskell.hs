import qualified Constraint.Checker as Constraint
import qualified Jigsaw.Jigsaw as Jigsaw
import qualified RT.RT as RT
import qualified Simulation.Simulation as Simulation
import qualified Editor.Editor as Editor
import qualified Pyramid.Pyramid as Pyramid
import System.Environment( getArgs )



-- | 'main', entry point. Execute the correct module wanted by the first argument.
main :: IO()
main = do
	args <- getArgs
	if length args < 1 -- If there is no arguments, we throw an error message.
	then putStrLn "Not enough arguments.\nUsage: ediT2-haskell <pattern's name> [arguments]"
	else main' args



-- | 'main'', Execute the correct module.
main' :: [String] -- ^ Arguments.
	-> IO ()
main' (pattern:args)
	| pattern == "Constraint" = Constraint.run args
	| pattern == "Jigsaw" = Jigsaw.run args
	| pattern == "RT" = RT.run args
	| pattern == "Simulation" = Simulation.run args
	| pattern == "Pyramid" = Pyramid.run args
	| pattern == "Editor" = Editor.run args
	| otherwise = putStrLn "Unknow pattern or command.\nPossibilities: RT, Jigsaw, Simulation, Pyramid, Editor, Constraint."