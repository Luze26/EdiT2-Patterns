-- | Module to generate .t2 file for the pattern Simulation.
module Simulation.Simulation
(
	run
) where


import Simulation.SimulationModel
import Util.Util
import qualified Util.KobbeComponents as K
import Util.TreeGenerator



-- | 'run', entry point. Expect a file path in argument pointing to a file containing information needed.
run :: [String] -> IO ()
run [] = putStrLn "Not enough arguments for simulation.\nUsage: ediT2-haskell Simulation <information file>"
run (fileInfo:_) = do
	text <- readFile fileInfo -- Read the file past in argument
	let (file, info) = readText text (\x -> read x :: Info) -- Extract information from the text. file = output file. info = information.
	let repart = repartitionGroups (length $ K.participantsObjects $ objects info) $ roles info -- Repartition by groups and roles for the simulation group activity.
	case repart of
		Possible _ -> do
			let (lvls, patternObjects) = generateLevels info -- lvls = list of levels, patternObjects = pattern objects.
			writeT2 file ["Activity","Group","Role","Participant","Resource"] (generate lvls) (showObjects patternObjects 1) 3
		NotPossible _ -> writeT2Err file [NotPossible "Can't do a good repartition in groups."]



-- | 'generateLevels', generatee the levels for the tree.
generateLevels :: Info -- ^ Information for the pattern.
	-> ([Level], K.PatternObjectsList) -- ^ [Level] = list of levels, 'PatternObjects' = pattern objects.
generateLevels info = (lvls, patternObject)
	where
		lvls = [generateActivityLvl, generateGroupLvl groups, generateRoleLvl nbGroups rolesNames, generateParticipantLvl participants repart,
			generateResourceLvl (resourcesProblem info) (roles info) nbGroups (length partObj) repart]  -- Levels.
		groups = createGroups nbGroups -- Groups for the simulation group activity.
		nbGroups = length repart -- Number of groups for the simulation group activity.
		rolesNames = K.rolesNames rolObj -- Names of roles.
		repart = possibleToList $ repartitionGroups (length partObj) $ roles info -- Repartition by groups and roles for the simulation group activity.
		patternObject = [activityObject, map (\g -> (g,"")) groups, rolObj, partObj, resObj] -- pattern object resulting of the generation.
		activityObject = [("Introduction",""),("Roles",""),("Groups simulation",""),("Simulation",""),("Share","")]
		participants = K.participantsLogins partObj -- Participants names.
		(_,_,partObj,resObj,rolObj) = objects info -- Pattern object given in information.



-- | 'generateActivityLvl', generate the activity level.
generateActivityLvl :: Level -- ^ The activity level.
generateActivityLvl = ("Activity", [[["Introduction"],["Roles"],["Groups simulation"],["Simulation"],["Share"]]])



-- | 'generateGroupLvl', generate the group level.
generateGroupLvl :: [String] -- ^ Groups for the group simulation activity.
	-> Level -- ^ The group's level.
generateGroupLvl groups = ("Group", [[[]], [[]], map (: []) groups, [[]], [[]]]) -- A "fake" node is needed for each activity without any groups.



-- | 'generateRoleLvl', generate the role level.
generateRoleLvl :: Int -- ^ Number of groups for the simulation group activity.
	-> [K.Role] -- ^ Roles' names.
	-> Level -- ^ The role's level.
generateRoleLvl nbGroup rolees = ("Role", [[]] : rolees' : replicate nbGroup rolees' ++ (map (: []) (createGroups nbGroup) : [[[]]]))
	where
		rolees' = map (: []) rolees



-- | 'generateParticipantLvl', generate the participant level.
generateParticipantLvl :: [K.Participant] -- ^ List of participants.
	-> [[Int]] -- ^ Repartition of participants for the simulation group activity.
	-> Level -- ^ Participant's level.
generateParticipantLvl ps repart = ("Participant", ps' : psRoles ++ psGroupsAndRoles ++ psSimuGroups ++ replicate 2 ps')
	where
		ps' = map (: []) ps -- Transform the list of participants in a list of list to match the fact that the content of a node is a list.
		psRoles = mergeRoles (replicate nbRoles []) psGroupsAndRoles nbRoles -- From the list of participants divided by groups and roles, we merge groups to have participants divided only by roles.
		psGroupsAndRoles = createParticipantsRoles psSimuGroups repart -- Divide simulation groups for the activity simulation, to groups also divided by roles.
		psSimuGroups = splitList ps' $ map sum repart -- Divide participants in groups with a correct number of actors.
		nbRoles = length $ head repart -- Number of roles.



-- | 'generateResourceLvl', generate the resource level.
generateResourceLvl :: [K.Resource] -- ^ List of resource for the problem.
	-> [RoleSimu] -- ^ List of roles.
	-> Int -- ^ Number of groups.
	-> Int -- ^ Number of participants.
	-> [[Int]] -- ^ Repartition by groups and roles for the simulation group activity.
	-> Level -- ^ Participant's level.
generateResourceLvl resPb rolees nbGroups nbParticipants repart =
	("Resource", replicate nbParticipants resPb' ++ resSimu ++ replicate (nbParticipants * 3) [[]])
	where
		resPb' = map (: []) resPb
		resSimu = createResourcesForRoles rolees $ foldl sumList [] repart



-- | 'createResourcesForRoles', create resources for each students for his role.
createResourcesForRoles :: [RoleSimu] -- ^ List of roles.
	-> [Int] -- ^ Number of participants per role.
	-> [[[K.Resource]]] -- ^ Resources for each students for his role.
createResourcesForRoles [] _ = []
createResourcesForRoles (role:roless) (repart:reparts) = (replicate repart $ map (: []) $ resources role) ++ createResourcesForRoles roless reparts



-- | 'mergeRoles', merge participants of the same role from groups of simulation group activity.
mergeRoles :: [[[K.Participant]]] -- ^ Participants divided by roles.
	-> [[[K.Participant]]] -- ^ Participants divided by groups and roles.
	-> Int -- ^ Number of roles.
	-> [[[K.Participant]]] -- ^ Participants divided by roles
mergeRoles acc [] _ = acc
mergeRoles acc groups nb = addAll (mergeRoles acc (drop nb groups) nb) $ take nb groups



-- | 'createParticipantsRoles', create the list of group of participants divided by roles.
createParticipantsRoles :: [[[K.Participant]]] -- ^ Participants divided in simulation groups.
	-> [[Int]] -- ^ Splits by role for each simulation group.
	-> [[[K.Participant]]] -- ^ Participants divided by role.
createParticipantsRoles [] _ = []
createParticipantsRoles (g:groups) (s:splits) = splitList g s ++ createParticipantsRoles groups splits



-- | 'createGroups', create groups for the simulation group activity.
createGroups :: Int -- ^ Number of groups.
	-> [String] -- ^ List of groups.
createGroups nb = [ "Group" ++ show i | i <- [1..nb]]



-- | 'repartitionGroups', give the number of participant needed per role per group.
repartitionGroups :: Int -- ^ The number of participants.
	-> [RoleSimu] -- ^ Roles.
	-> Possible [[Int]] -- ^ Repartition.
repartitionGroups nbP rolees = repartition2Multiple nbP (map (\r -> (nbActor r, above r, below r)) rolees)