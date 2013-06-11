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
		above :: Int,
		below :: Int,
		nbG :: Int -- ^ Number of groups
	} deriving (Read)



-- | 'run', entry point. Expect a file path in argument pointing to a file containing information needed.
run :: [String] -> String
run [] = putStrLn "Not enough arguments for pyramid.\nUsage: ediT2-haskell Pyramid <information file>"
run (fileInfo:_) = do
	text <- readFile fileInfo -- Read the file past in argument
	let (file, info) = readText text (\x -> read x :: Info) -- Extract information from the text. file = output file. info = information.
	let repart = Possible []
	case repart of
		Possible _ -> do
			let (lvls, patternObjects) = generateLevels info -- lvls = list of levels, patternObjects = pattern objects.
			writeT2 file ["Activity","Group","Resource","Participant"] (generate lvls) (showObjects patternObjects) -- Write a file, with the generated tree and the pattern object.
		NotPossible _ -> writeT2Err file [NotPossible "Error"]




-- | 'generateLevels', generatee the levels for the tree.
generateLevels :: Info -- ^ Information for the pattern.
	-> ([Level], PatternObjects) -- ^ [Level] = list of levels, 'PatternObjects' = pattern objects.
generateLevels info = (lvls, (activityObjects, map (\g -> (g,g)) groups, partObj, resObj, roleObjects))
	where
		lvls = [generateActivityLvl nbLvls] -- [Level], levels.
		nbLvls = nbLvl info
		
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
generateActivityLvl :: Int -- ^ Number of pyramid's levels.
	-> Level -- ^ The activity level.
generateActivityLvl nbLvls = ("Activity", [ [["Level " ++ show i]] | i <- [1..nb] ])



-- | 'generateResourceLvl', generate the resource level.
generateResourceLvl :: [Resource] -- ^ List of resources corresponding to passages.
	-> Level -- ^ The resource's level.
generateResourceLvl res = ("Resource", [[[]], map (: []) res]) -- A "fake" node is needed for the first activity where there isn't any resources.



-- | 'generateGroupLvl', generate the group level.
generateGroupLvl :: Int -- ^ Number of passages (= number of resources).
	-> [String] -- ^ List of groups for the learning activity.
	-> Level -- ^ The group's level.
generateGroupLvl nb gs = ("Group", [[]] : replicate nb (map (: []) gs)) -- A "fake" node is needed for the first activity where there isn't any groups.
