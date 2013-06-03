import Tree
import Util
import TreeGenerator
import System.Environment( getArgs )



data Info =
	Info {
		objects :: PatternObjects,
		nbPPG :: Int,
		above :: Int,
		below :: Int
	}



main :: IO()
main = do
	args <- getArgs
	text <- readFile $ head args
	let (file, info) = readText $ lines text
	let (lvls, patternObjects) = generateLevels info
	writeT2 file ["Activity","Resource","Group","Role","Participant"] (generate lvls) patternObjects



-- generateLevels, generatee the levels of the tree
-- @[Level] -> levels
generateLevels :: Info -> ([Level], PatternObjects)
generateLevels info = (lvls, (activityObjects, map (\g -> (g,g)) groups, participantsObjects po, resourcesObjects po, []))
	where
		lvls = generateActivityLvl : (generateResourceLvl resources) : groupLvl : []
		groupLvl = generateGroupLvl (length resources) $ groups
		resources = resourcesNames $ resourcesObjects po
		activityObjects = [("Introduction", "Introduction"), ("Learning", "Learning")]
		groups = createGroups $ length repart
		repart =  repartitionParticipant (length participants) (nbPPG info) (above info) (below info)
		participants = participantsLogins $ participantsObjects po
		po = objects info



generateActivityLvl :: Level
generateActivityLvl = ("Activity", [[["Introduction"]],[["Learning"]]])



generateResourceLvl :: [Resource] -> Level
generateResourceLvl res = ("Resource", [] : (map (\r -> [r]) res) : [])



generateGroupLvl :: Int -> [String] -> Level
generateGroupLvl nb gs = ("Group", [] : (replicate nb $ map (\g -> [g]) gs))



createGroups :: Int -> [String]
createGroups nb = ["Group " ++ (show i) | i <- [1..nb]]



repartitionParticipant :: Int -> Int -> Int -> Int -> [Int]
repartitionParticipant nbP nbPPG above below = possibleToList $ repartition nbP nbPPG above below



readText :: [String] -> (String, Info)
readText linees = (file, Info objects (readInt (linees !! 1)) (readInt (linees !! 2)) (readInt (linees !! 3)))
	where
		(file, objects) = readObjects $ head linees