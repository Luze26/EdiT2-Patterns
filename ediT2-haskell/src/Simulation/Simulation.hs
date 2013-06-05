import SimulationModel
import qualified Util.Util as U
import Util.TreeGenerator
import System.Environment( getArgs )



-- | 'main', entry point. Expect a file path in argument, pointing to a file containing information needed.
main :: IO()
main = do
	args <- getArgs
	text <- readFile $ head args -- Read the file past in argument
	let (file, info) = U.readText text (\x -> read x :: Info) -- Extract information from the text. file = output file. info = information.
	let (lvls, patternObjects) = generateLevels info -- lvls = list of levels, patternObjects = pattern objects.
	let repart = repartitionGroups (length $ U.participantsObjects $ objects info) $ roles info -- Repartition by groups and roles for the simulation group activity.
	case repart of
		U.Possible _ -> U.writeT2 file ["Activity","Group","Role","Participant","Resource"] (generate lvls) patternObjects
		U.NotPossible _ -> U.writeT2Err file [U.NotPossible "Can't do a good repartition in groups."]



-- | 'generateLevels', generatee the levels for the tree.
generateLevels :: Info -- ^ Information for the pattern.
	-> ([Level], U.PatternObjects) -- ^ [Level] = list of levels, 'PatternObjects' = pattern objects.
generateLevels info = (lvls, patternObject)
	where
		lvls = generateActivityLvl : generateGroupLvl groups : generateRoleLvl nbGroups rolesNames : generateParticipantLvl participants repart : []  -- Levels.
		groups = createGroups nbGroups -- Groups for the simulation group activity.
		nbGroups = length repart -- Number of groups for the simulation group activity.
		rolesNames = U.rolesNames rolObj -- Names of roles.
		repart = U.possibleToList $ repartitionGroups (length partObj) $ roles info -- Repartition by groups and roles for the simulation group activity.
		patternObject = (activityObject, map (\g -> (g,"")) groups, partObj, resObj, rolObj) -- pattern object resulting of the generation.
		activityObject = [("Introduction",""),("Roles",""),("Groups simulation",""),("Simulation",""),("Share","")]
		participants = U.participantsLogins partObj -- Participants names.
		(_,_,partObj,resObj,rolObj) = objects info -- Pattern object given in information.



-- | 'generateActivityLvl', generate the activity level.
generateActivityLvl :: Level -- ^ The activity level.
generateActivityLvl = ("Activity", [[["Introduction"]],[["Roles"]],[["Groups simulation"]],[["Simulation"]],[["Share"]]])



-- | 'generateGroupLvl', generate the group level.
generateGroupLvl :: [String] -- ^ Groups for the group simulation activity.
	-> Level -- ^ The group's level.
generateGroupLvl groups = ("Group", [["Group???"]] : [["Group???"]] : (map (\g -> [g]) groups) : [[["Group???"]], [["Group???"]]]) -- A "fake" node is needed for each activity without any groups.



-- | 'generateRoleLvl', generate the role level.
generateRoleLvl :: Int -- ^ Number of groups for the simulation group activity.
	-> [U.Role] -- ^ Roles' names.
	-> Level -- ^ The role's level.
generateRoleLvl nbGroup rolees = ("Role", [["Role???"]] : rolees' : replicate nbGroup rolees' ++ (map (\g -> [g]) (createGroups nbGroup) : [[["Role???"]]]))
	where
		rolees' = map (\r -> [r]) rolees



-- | 'generateParticipantLvl', generate the participant level.
generateParticipantLvl :: [U.Participant] -- ^ List of participants.
	-> [[Int]] -- ^ Repartition of participants for the simulation group activity.
	-> Level -- ^ Participant's level.
generateParticipantLvl ps repart = ("Participant", ps' : psRoles ++ psGroupsAndRoles ++ psSimuGroups ++ replicate 2 ps')
	where
		ps' = map (\p -> [p]) ps -- Transform the list of participants in a list of list to match the fact that the content of a node is a list.
		psRoles = mergeRoles (replicate nbRoles []) psGroupsAndRoles nbRoles -- From the list of participants divided by groups and roles, we merge groups to have participants divided only by roles.
		psGroupsAndRoles = createParticipantsRoles psSimuGroups repart -- Divide simulation groups for the activity simulation, to groups also divided by roles.
		psSimuGroups = U.splitList ps' $ map (\g -> sum g) repart -- Divide participants in groups with a correct number of actors.
		nbRoles = length $ head repart -- Number of roles.



-- | 'mergeRoles', merge participants of the same role from groups of simulation group activity.
mergeRoles :: [[[U.Participant]]] -- ^ Participants divided by roles.
	-> [[[U.Participant]]] -- ^ Participants divided by groups and roles.
	-> Int -- ^ Number of roles.
	-> [[[U.Participant]]] -- ^ Participants divided by roles
mergeRoles acc [] _ = acc
mergeRoles acc groups nb = U.addAll (mergeRoles acc (drop nb groups) nb) $ take nb groups



-- | 'createParticipantsRoles', create the list of group of participants divided by roles.
createParticipantsRoles :: [[[U.Participant]]] -- ^ Participants divided in simulation groups.
	-> [[Int]] -- ^ Splits by role for each simulation group.
	-> [[[U.Participant]]] -- ^ Participants divided by role.
createParticipantsRoles [] _ = []
createParticipantsRoles (g:groups) (s:splits) = U.splitList g s ++ createParticipantsRoles groups splits



-- | 'createGroups', create groups for the simulation group activity.
createGroups :: Int -- ^ Number of groups.
	-> [String] -- ^ List of groups.
createGroups nb = [ "Group" ++ (show i) | i <- [1..nb]]



-- | 'repartitionGroups', give the number of participant needed per role per group.
repartitionGroups :: Int -- ^ The number of participants.
	-> [RoleSimu] -- ^ Roles.
	-> U.Possible [[Int]] -- ^ Repartition.
repartitionGroups nbP rolees = U.repartition2Multiple nbP (map (\r -> (nbActor r, above r, below r)) rolees)