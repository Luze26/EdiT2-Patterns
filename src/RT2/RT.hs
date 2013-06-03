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
generateLevels info = (lvls, (activityObjects, map (\g -> (g,g)) groups, participantsObjects po, resourcesObjects po, roleObjects))
	where
		lvls = generateActivityLvl : (generateResourceLvl resources) : groupLvl : (generateRoleLvl $ (*) nbResources $ length groups) :
			generateParticipantLvl participants repart nbResources : []
		groupLvl = generateGroupLvl nbResources groups
		resources = resourcesNames $ resourcesObjects po
		activityObjects = [("Introduction", ""), ("Learning", "")]
		roleObjects = [("Teacher", ""), ("Student", "")]
		groups = createGroups $ length repart
		repart =  repartitionParticipant (length participants) (nbPPG info) (above info) (below info)
		participants = participantsLogins $ participantsObjects po
		nbResources = length resources
		po = objects info



generateActivityLvl :: Level
generateActivityLvl = ("Activity", [[["Introduction"]],[["Learning"]]])



generateResourceLvl :: [Resource] -> Level
generateResourceLvl res = ("Resource", [["Resource???"]] : (map (\r -> [r]) res) : [])



generateGroupLvl :: Int -> [String] -> Level
generateGroupLvl nb gs = ("Group", [["Group???"]] : (replicate nb $ map (\g -> [g]) gs))



generateRoleLvl :: Int -> Level
generateRoleLvl nbGroup = ("Role", [["Role???"]] : (replicate nbGroup [["Teacher"],["Student"]]))


generateParticipantLvl :: [Participant] -> [Int] -> Int -> Level
generateParticipantLvl participants repart nbResources = ("Participant", (map (\p -> [p]) participants) :
	(roundRobin nbResources $ splitList participants repart))



roundRobin :: Int -> [[Participant]] -> [[[Participant]]]
roundRobin 0 _ = []
roundRobin i participants = (foldl (\acc g -> [[head g]] : [tail g] : acc) [] turned) ++ roundRobin (i-1) turned
	where
		turned = turn participants



turn :: [[Participant]] -> [[Participant]]
turn [] = []
turn ((p:ps):pss) = (ps ++ [p]) : turn pss



createGroups :: Int -> [String]
createGroups nb = ["Group " ++ (show i) | i <- [1..nb]]



repartitionParticipant :: Int -> Int -> Int -> Int -> [Int]
repartitionParticipant nbP nbPPG above below = possibleToList $ repartition nbP nbPPG above below



readText :: [String] -> (String, Info)
readText linees = (file, Info objects (readInt (linees !! 1)) (readInt (linees !! 2)) (readInt (linees !! 3)))
	where
		(file, objects) = readObjects $ head linees