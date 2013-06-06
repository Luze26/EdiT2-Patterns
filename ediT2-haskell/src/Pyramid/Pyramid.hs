import Util.Util
import Util.TreeGenerator
import System.Environment( getArgs )



data Info =
	-- | 'Info', contain information needed for the pattern.
	Info {
		objects :: PatternObjects, -- ^ Pattern's objects.
		nbLvl :: Int, -- ^ Number of levels for the pyramid.
		nbPPG :: Int, -- ^ Number of participants per group preferred.
		nbG :: Int -- ^ Number of groups
	} deriving (Read)



-- | 'main', entry point. Expect a file path in argument pointing to a file containing information needed.
main :: IO()
main = do
	args <- getArgs
	text <- readFile $ head args -- Read the file past in argument
	let (file, info) = readText text (\x -> read x :: Info) -- Extract information from the text. file = output file. info = information.
	