-- | Module to generate .t2 file for the pattern Pyramid.
module Pyramid (
	run
) where


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



-- | 'run', entry point. Expect a file path in argument pointing to a file containing information needed.
run :: [String] -> String
run [] = putStrLn "Not enough arguments for pyramid.\nUsage: ediT2-haskell Pyramid <information file>"
run (fileInfo:_) = do
	text <- readFile fileInfo -- Read the file past in argument
	let (file, info) = readText text (\x -> read x :: Info) -- Extract information from the text. file = output file. info = information.
	