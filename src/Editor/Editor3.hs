import System.Environment( getArgs )
import Util
import Tree
import TreeGenerator



-- Generate, entry point. Create the participant's list
main :: IO()
main = do
	args <- getArgs
	start (args ++ [createList 20])



-- start, parse arguments and write generated tree in a file
-- @[String] -> args
start :: [String] -> IO()
start (file:nbSec:e:ps:_) = writeFile file $ showTree ["Topic","Editor","Section","Editor2"] $ generate $ generateLevels (read ps::[String]) (readInt nbSec) $ readInt e



generateLevels :: [String] -> Int -> Int -> [Level]
generateLevels participants nbSec nbSecEditor = generateTopics participants : generateLvlEditor participants

generateLvlTopics :: [Participant] -> Level
generateLvlTopics participants = ("Topic", generateLvlTopics' participants 1)

generateLvlTopics' :: [Participant] -> Int -> [[[String]]]
generateLvlTopics' [] _ = []
generateLvlTopics' (_:ts) i = [["Topic" ++ show i]] : generateLvlTopics' ts (i+1)

generateLvlEditor :: [Participant] -> Level
generateLvlEditor participants = ("Editor", generateLvlEditor' participants)

generateLvlEditor' :: [Participant] -> [[[String]]]
generateLvlEditor' [] = []
generateLvlEditor' p:ps = [[p]] : generateLvlEditor' ps

generateLvlSection :: [Participant] -> Int -> Level
generateLvlSection participants nbS = ("Section", generateLvlSection' participants nbS 1)


generateLvlSection' :: [Participant] -> Int -> Int -> [[[String]]]
generateLvlSection' [] _ _ = []
generateLvlSection' (_:ps) nbS i = generateLvlSection'' nbS i 1 : (generateLvlSection' ps nbS (i+1))


generateLvlSection'' :: Int -> Int -> Int -> [[String]]
generateLvlSection'' nbS i i2
	| i2 <= nbS = ["Section" ++ (div i nbS) ++ (show i2)] : generateLvlSection'' nbS i (i2+1)
	| otherwise = []


--generateLvlEditor2 :: [Participant] -> Level
--generateLvlEditor participants = ("Editor2", )

