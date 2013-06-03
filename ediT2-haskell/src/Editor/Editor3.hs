import System.Environment( getArgs )
import Util
import Tree
import TreeGenerator



-- Generate, entry point. Create the participant's list
main :: IO()
main = do
	args <- getArgs
	start (args ++ [createList 6])



-- start, parse arguments and write generated tree in a file
-- @[String] -> args
start :: [String] -> IO()
start (file:nbSec:e:ps:_) = writeFile file $ showTree ["Topic","Editor","Section","Editor2"]  $ generate $ generateLevels (read ps::[String]) (readInt nbSec) $ readInt e



generateLevels :: [String] -> Int -> Int -> [Level]
generateLevels participants nbSec nbSecEditor = generateLvlTopics participants :
		 generateLvlEditor participants : generateLvlSection participants nbSec : generateLvlEditor2 participants nbSecEditor nbSec : []

generateLvlTopics :: [Participant] -> Level
generateLvlTopics participants = ("Topic", generateLvlTopics' participants 1)

generateLvlTopics' :: [Participant] -> Int -> [[[String]]]
generateLvlTopics' [] _ = []
generateLvlTopics' (_:ts) i = [["Topic" ++ show i]] : generateLvlTopics' ts (i+1)

generateLvlEditor :: [Participant] -> Level
generateLvlEditor participants = ("Editor", generateLvlEditor' participants)

generateLvlEditor' :: [Participant] -> [[[String]]]
generateLvlEditor' [] = []
generateLvlEditor' (p:ps) = [[p]] : generateLvlEditor' ps

generateLvlSection :: [Participant] -> Int -> Level
generateLvlSection participants nbS = ("Section", generateLvlSection' participants nbS 1)


generateLvlSection' :: [Participant] -> Int -> Int -> [[[String]]]
generateLvlSection' [] _ _ = []
generateLvlSection' (_:ps) nbS i = generateLvlSection'' nbS i 1 : (generateLvlSection' ps nbS (i+1))


generateLvlSection'' :: Int -> Int -> Int -> [[String]]
generateLvlSection'' nbS i i2
	| i2 <= nbS = ["Section" ++ (show i ) ++ (show i2)] : generateLvlSection'' nbS i (i2+1)
	| otherwise = []


generateLvlEditor2 :: [Participant] -> Int -> Int -> Level
generateLvlEditor2 participants nbE nbSec = ("Editor2", generateLvlEditor2' nbE nbSec participants $ take ((nbE+3)*(length participants)) $ cycle participants)


generateLvlEditor2' :: Int -> Int -> [Participant] -> [Participant] -> [[[Participant]]]
generateLvlEditor2' _ _ [] _ = []
generateLvlEditor2' nbE nbSec (p:ps) participants = (map (\e -> [e]) editors) : (map (\e -> [e]) editors2) : generateLvlEditor2' nbE nbSec ps ps3
	where
		(editors, ps2) = takeParticipants participants p nbE
		(editors2, ps3) = takeParticipants ps2 p nbE


takeParticipants :: [Participant] -> Participant -> Int -> ([Participant], [Participant])
takeParticipants ps _ 0 = ([],ps)
takeParticipants (p:ps) p2 nbE
	| p /= p2 = let (editors, ps2) = takeParticipants ps p2 (nbE-1) in (p:editors,ps2)
	| otherwise = takeParticipants ps p2 nbE
