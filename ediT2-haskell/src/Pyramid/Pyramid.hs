-- | Module to generate .t2 file for the pattern Pyramid.
module Pyramid.Pyramid (
	run
) where


import Util.Util
import Util.TreeGenerator
import Util.KobbeComponents



data Info =
	-- | 'Info', contain information needed for the pattern.
	Info {
		objects :: PatternObjects, -- ^ Pattern's objects.
		nbLvl :: Int, -- ^ Number of levels for the pyramid.
		nbPPG :: Int, -- ^ Number of participants per group preferred.
		above :: Int,
		below :: Int,
		output :: String
	} deriving (Read)



-- | 'run', entry point. Expect a file path in argument pointing to a file containing information needed.
run :: [String] -> IO()
run [] = putStrLn "Not enough arguments for pyramid.\nUsage: ediT2-haskell Pyramid <information file>"
run (fileInfo:_) = do
	text <- readFile fileInfo -- Read the file past in argument
	let (file, info) = readText text (\x -> read x :: Info) -- Extract information from the text. file = output file. info = information.
	let repart = repartition  (length $ participantsObjects $ objects info) (nbPPG info) (above info) (below info) True
	case repart of
		Possible _ -> do
			let (lvls, patternObjects) = generateLevels info -- lvls = list of levels, patternObjects = pattern objects.
			writeT2 file ["Activity","Group","Resource","Participant"] (generate lvls) (showObjects patternObjects 1) 3 -- Write a file, with the generated tree and the pattern object.
		NotPossible _ -> writeT2Err file [NotPossible "Error"]




-- | 'generateLevels', generatee the levels for the tree.
generateLevels :: Info -- ^ Information for the pattern.
	-> ([Level], PatternObjectsList) -- ^ [Level] = list of levels, 'PatternObjects' = pattern objects.
generateLevels info = (lvls, [activityObjects, [], [], [], []])
	where
		lvls = [generateActivityLvl nbLvls, generateGroupLvl info repart, generateResourceLvl info repart, generateParticipantLvl participants nbGroupPerLvls] -- [Level], levels.
		nbLvls = nbLvl info
		nbParticipants = length partObj
		nbGroupPerLvls = nbGroupPerLvl nbLvls (length repart)
		participants = participantsLogins partObj
		activityObjects = [ ("Level " ++ show i, "") | i <- [1..nbLvls]]
		repart = possibleToList $ repartition  nbParticipants (nbPPG info) (above info) (below info) True
		(_,_,partObj,_,_) = objects info -- Pattern object partially filled, given by the information file.



-- | 'generateActivityLvl', generate the activity level.
generateActivityLvl :: Int -- ^ Number of pyramid's levels.
	-> Level -- ^ The activity level.
generateActivityLvl nbLvls = ("Activity", [ [["Level " ++ show i]] | i <- [1..nbLvls] ])




-- | 'generateGroupLvl', generate the group level.
generateGroupLvl :: Info -- ^ Pattern's information.
	-> [Int] -- ^ Repartition.
	-> Level -- ^ The group's level.
generateGroupLvl info repart = ("Group", map (map (: [])) $ createGroups (nbGroupPerLvl (nbLvl info) (length repart)) [] True)




-- | 'generateResourceLvl', generate the resource level.
generateResourceLvl :: Info -- ^ Pattern's information.
	-> [Int] -- ^ Repartition.
	-> Level -- ^ The resource's level.
generateResourceLvl info repart = ("Resource", map (\g ->  [(g ++ "." ++ output info) : resources]) $
	concat $ createGroups (nbGroupPerLvl (nbLvl info) (length repart)) [] True)
	where
		resources = resourcesNames $ resourcesObjects $ objects info



-- | 'generateParticipantLvl', generate the participant level.
generateParticipantLvl :: [Participant] -- ^ Participants.
	-> [Int] -- ^ Repartition.
	-> Level -- ^ The participant's level.
generateParticipantLvl participants repart = ("Participant", createParticipants participants repart)



createParticipants :: [Participant] -- ^ Participants.
	-> [Int] -- ^ Repartition.
	-> [[[Participant]]]
createParticipants _ [] = []
createParticipants participants (l:lvls) = createParticipants' participants nb ++ createParticipants participants lvls
	where
		nb = div (length participants) l



createParticipants' :: [Participant] -- ^ Participants.
	-> Int -- ^ Size of groups.
	-> [[[Participant]]]
createParticipants' [] _ = []
createParticipants' participants nb = [take nb participants] : createParticipants' (drop nb participants) nb



-- | 'createGroups', create groups for each level.
createGroups :: [Int] -- ^ Number of groups per level.
	-> [String] -- ^ Id of last groups.
	-> Bool -- ^ 'True', if first call.
	-> [[String]] -- ^ Groups.
createGroups [] _ _ = []
createGroups (nb:nbs) ids first = map ("Group " ++) newIds :createGroups nbs newIds False
	where
		newIds
			| not first = createGroups' (div (length ids) nb) ids
			| otherwise = map show [1..nb]



-- | 'createGroups'', create groups for a given level.
createGroups' :: Int -- ^ Size of fusion.
	-> [String] -- ^ Groups of the level below.
	-> [String] -- ^ Groups.
createGroups' _ [] = []
createGroups' nb ids = init (concatMap (++ "+") (take nb ids)) : createGroups' nb (drop nb ids)



-- | 'nbGroupPerLvl', return the number of group wanted per level.
nbGroupPerLvl :: Int -- ^ Number of levels.
	-> Int -- ^ Number of groups for the first level.
	-> [Int] -- ^ Number of groups for each level.
nbGroupPerLvl nbLvls nbGroups
	| nbLvls > 2 && nbLvls >= nbGroups = nbGroups : nbGroupPerLvl (nbLvls-1) nbGroups
	| nbLvls == 1 = [1]
	| otherwise = newNbGroups : nbGroupPerLvl (nbLvls-1) newNbGroups
	where
		newNbGroups = div nbGroups (div nbGroups nbLvls)