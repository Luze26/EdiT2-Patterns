-- | Module to generate .t2 file for the pattern Reciprocal Teaching.
module RT.RT
(
	run
) where


import Util.Util
import Util.TreeGenerator



data Info =
	-- | 'Info', contain information needed for the pattern.
	Info {
		objects :: PatternObjects, -- ^ Pattern's objects.
		nbPPG :: Int, -- ^ Number of participants per group preferred.
		above :: Int, -- ^ Above margin.
		below :: Int, -- ^ Below margin.
		uniform :: Bool -- ^ Uniform repartition over closest one, if True.
	} deriving (Read)



-- | 'run', entry point. Expect a file path in argument pointing to a file containing information needed.
run :: [String] -> IO ()
run [] = putStrLn "Not enough arguments for reciprocal teaching.\nUsage: ediT2-haskell RT <information file>"
run (fileInfo:_) = do
	text <- readFile fileInfo -- Read the file past in argument
	let (file, info) = readText text (\x -> read x :: Info) -- Extract information from the text. file = output file. info = information.
	let repart = repartitionParticipant (length $ participantsObjects $ objects info) (nbPPG info) (above info) (below info) True -- Sizes repartition for groups.
	case repart of
		Possible _ -> do
			let (lvls, patternObjects) = generateLevels info -- lvls = list of levels, patternObjects = pattern objects.
			writeT2 file ["Activity","Resource","Group","Role","Participant"] (generate lvls) patternObjects -- Write a file, with the generated tree and the pattern object.
		NotPossible _ -> writeT2Err file [NotPossible "Can't do a good repartiton of participants for the learning activity."]



-- | 'generateLevels', generatee the levels for the tree.
generateLevels :: Info -- ^ Information for the pattern.
	-> ([Level], PatternObjects) -- ^ [Level] = list of levels, 'PatternObjects' = pattern objects.
generateLevels info = (lvls, (activityObjects, map (\g -> (g,g)) groups, partObj, resObj, roleObjects))
	where
		lvls = generateActivityLvl : (generateResourceLvl resources) : groupLvl : (generateRoleLvl $ (*) nbResources $ length groups) :
			generateParticipantLvl participants repart nbResources : [] -- [Level], levels.
		groupLvl = generateGroupLvl nbResources groups -- The level for the group notion.
		resources = resourcesNames resObj -- Resources names.
		activityObjects = [("Introduction", ""), ("Learning", "")] -- Activity object for the pattern object.
		roleObjects = [("Teacher", ""), ("Student", "")] -- Role object for the pattern object.
		groups = createGroups $ length repart -- Groups created for the second activity (Learning).
		repart = possibleToList $ repartitionParticipant (length participants) (nbPPG info) (above info) (below info) (uniform info) -- Reparition of the participants, for groups in the Learning activity.
		participants = participantsLogins partObj -- Participants logins.
		nbResources = length resources -- Number of resources.
		(_,_,partObj,resObj,_) = objects info -- Pattern object partially filled, given by the information file.



-- | 'generateActivityLvl', generate the activity level.
generateActivityLvl :: Level -- ^ The activity level.
generateActivityLvl = ("Activity", [[["Introduction"]],[["Learning"]]])



-- | 'generateResourceLvl', generate the resource level.
generateResourceLvl :: [Resource] -- ^ List of resources corresponding to passages.
	-> Level -- ^ The resource's level.
generateResourceLvl res = ("Resource", [["Resource???"]] : (map (\r -> [r]) res) : []) -- A "fake" node is needed for the first activity where there isn't any resources.



-- | 'generateGroupLvl', generate the group level.
generateGroupLvl :: Int -- ^ Number of passages (= number of resources).
	-> [String] -- ^ List of groups for the learning activity.
	-> Level -- ^ The group's level.
generateGroupLvl nb gs = ("Group", [["Group???"]] : (replicate nb $ map (\g -> [g]) gs)) -- A "fake" node is needed for the first activity where there isn't any groups.



-- | 'generateRoleLvl', generate the role level.
generateRoleLvl :: Int -- ^ Number of groups for the learning activity.
	-> Level -- ^ The role's level.
generateRoleLvl nbGroup = ("Role", [["Role???"]] : (replicate nbGroup [["Teacher"],["Student"]])) -- A "fake" node is needed for the first activity where there isn't any roles.



-- | 'generateParticipantLvl', generate the participant level.
generateParticipantLvl :: [Participant] -- ^ List of participants.
	-> [Int] -- ^ Repartition of participants for the learning activity.
	-> Int  -- ^ Number of passages (= number of resources).
	-> Level -- ^ Participant's level.
generateParticipantLvl participants repart nbResources = ("Participant", (map (\p -> [p]) participants) :
	(roundRobin nbResources $ splitList participants repart)) -- It creates the list for the first activity where participants are togethere. And for each passage, it turns the list of participant for each group.



-- | 'roundRobin', create the part of the participant's level for the learning activity.
-- It turns the list of participants for each group, and take the first participant apart to be the "teacher".
roundRobin :: Int -- ^ Number of time that it must turns the list (= number of passages = number of resources).
	-> [[Participant]] -- ^ Lists of participants already splitted in groups.
	-> [[[Participant]]] -- ^ The part of the participant's level for the learning activity.
roundRobin 0 _ = []
roundRobin i participants = (foldl (\acc g -> [[head g]] : [tail g] : acc) [] turned) ++ roundRobin (i-1) turned
	where
		turned = turn participants -- Groups of participants turned.



-- | 'turn', put the last participant of each group in the first place.
turn :: [[Participant]] -> [[Participant]]
turn [] = []
turn ((p:ps):pss) = (ps ++ [p]) : turn pss



-- | 'createGroups', create groups for the learning level.
createGroups :: Int -- ^ Number of groups.
	-> [String] -- ^ List of groups.
createGroups nb = ["Group " ++ (show i) | i <- [1..nb]]



-- | 'repartitionParticipant', size repartition for groups of the learning level.
repartitionParticipant :: Int -- ^ Number of participants.
	-> Int -- ^ Number of participants per group wanted.
	-> Int -- ^ Above margin.
	-> Int -- ^ Below margin.
	-> Bool -- ^ If the user prefers a uniform repartition or a closest repartition.
	-> Possible [Int] -- ^ Possible size's repartition.
repartitionParticipant nbP nbPPG above below uniform = repartition nbP nbPPG above below uniform