-- | Module to generate .t2 file for the pattern Jigsaw.
module Jigsaw.Jigsaw (
	run
) where


import Util.Util
import Util.TreeGenerator
import Util.KobbeComponents
import qualified Jigsaw.JigsawModel as M



-- | 'run', entry point. Expect a file path in argument pointing to a file containing information needed.
run :: [String] -> IO ()
run [] = putStrLn "Not enough arguments for jigsaw.\nUsage: ediT2-haskell Jigsaw <information file>"
run (fileInfo:_) = do
	text <- readFile fileInfo -- Read the file past in argument
	let (file, info) = readText text (\x -> read x :: M.Info) -- Extract information from the text. file = output file. info = information.
	let noPoss = notPossible $ repartitionJigsaw (participantsList info) (M.themes info) : repartitionInitial info
	if null noPoss
		then do let (lvls, patternObjects) = generateLevels info -- lvls = list of levels, patternObjects = pattern objects.
			writeT2 file ["Activity","Group","Participant","Resource"] (generate lvls) (showObjects patternObjects) -- Write a fil
		else writeT2Err file noPoss



-- | 'notPossible', return the list of not possible actions.
notPossible :: [Possible a] -- ^ Possible actions.
	-> [Possible String] -- ^ Not possible actions.
notPossible = foldl (\acc x -> case x of NotPossible msg -> NotPossible msg : acc; Possible _ -> acc) []



-- | 'generateLevels', generatee the levels of the tree.
generateLevels :: M.Info -- ^ Pattern information.
	-> ([Level], PatternObjects) -- ^ Levels and pattern objects generated.
generateLevels info = (lvls, patternObjects')
	where
		lvls = [generateActivitiesLvl, generateGroupsLvl info, generateParticipantsLvl info, generateResourcesLvl info]
		patternObjects' = (actObj,groupObj,partObj,resObj,rolObj)
		groupObj = groupObjectsList info
		(actObj,_,partObj,resObj,rolObj) = M.objects info


-- //////////////////////////////////// ACTIVITIES ////////////////////////////////////

-- | 'generateActivitiesLvl', generate activities level.
generateActivitiesLvl :: Level -- ^ Activities level.
generateActivitiesLvl = ("Activity", [[["Initial"]], [["Expert"]], [["Jigsaw"]]]) 



-- //////////////////////////////////// GROUPS ////////////////////////////////////

-- | 'generateGroupsLvl', generate groups level.
generateGroupsLvl :: M.Info -- ^ Pattern information.
	-> Level -- ^ Groups level.
generateGroupsLvl info = ("Group", [createGroupsInitial info, createGroupsExpert (M.themes info), createGroupsJigsaw info])



-- | 'createGroupsInitial', create initial groups
createGroupsInitial :: M.Info -- ^ Pattern information.
	-> [[Group]] -- ^ Initial groups.
createGroupsInitial info = [["Group " ++ show i] | i <- [1..nbGroupsInitial info]]



-- | 'createGroupsExpert', create expert groups.
createGroupsExpert :: [M.Theme] -- ^ Themes.
	-> [[Group]] -- ^ Expert groups.
createGroupsExpert themes = [["Group Expert " ++ M.name t] | t <- themes]



-- | 'createGroupsJigsaw', create jigsaw groups.
createGroupsJigsaw :: M.Info -- ^ Pattern information.
	-> [[Group]] -- ^ Jigsaw groups.
createGroupsJigsaw info = [["Group Jigsaw " ++ show i] | i <- [1..(length $ createParticipantsJigsaw (participantsList info) (M.themes info))]]



-- //////////////////////////////////// PARTICIPANTS ////////////////////////////////////

-- | 'generateParticipantsLvl', generate participants level.
generateParticipantsLvl :: M.Info -- ^ Pattern information.
	-> Level -- ^ Participants level.
generateParticipantsLvl info = ("Participant", map (map (: [])) (createParticipantsInitial info ++ createParticipantsExpert ps themes ++ createParticipantsJigsaw ps themes))
	where
		ps = participantsList info
		themes = M.themes info



-- | 'createParticipantsInitial', split participants into initial groups
createParticipantsInitial :: M.Info -- ^ Pattern information.
	-> [M.InitialGroup] -- ^ Initial groups.
createParticipantsInitial info = map snd $ createParticipantsInitialWithTheme info




-- | 'createParticipantsInitialWithTheme', split participants into initial groups.
createParticipantsInitialWithTheme :: M.Info -- ^ Pattern information.
	-> [(M.Theme, M.InitialGroup)] -- ^ Initial groups with their associated theme.
createParticipantsInitialWithTheme info = createParticipantsInitial' (createParticipantsExpert (participantsList info) (M.themes info)) 
	(M.themes info) $ map possibleToList $ repartitionInitial info



-- | 'createParticipantsInitial'', split participants into initial groups from expert groups.
createParticipantsInitial' :: [M.ExpertGroup] -- ^ Expert groups.
	-> [M.Theme] -- ^ Themes.
	-> [[Int]] -- ^ Repartition, for each expert groups in initial groups.
	-> [(M.Theme, M.InitialGroup)] -- ^ Initial groups with their associated theme.
createParticipantsInitial' [] _ _ = []
createParticipantsInitial' (g:groups) (t:themes) (r:rs) = map (\x -> (t,x)) (splitList g r) ++ createParticipantsInitial' groups themes rs



-- | 'createParticipantsExpert', split participants into expert groups.
-- Merge participants splited per themes for each jigsaw group, into a list of participant splited only per themes
createParticipantsExpert :: [Participant] -- ^ Participants.
	-> [M.Theme] -- ^ Themes.
	-> [M.ExpertGroup] -- ^ Expert groups.
createParticipantsExpert participants themes =
	foldl addAll (replicate (length themes) []) (createParticipantsExpert' (createParticipantsJigsaw participants themes) themes)



-- | 'createParticipantsExpert'', split jigsaw groups per themes
createParticipantsExpert' :: [M.JigsawGroup] -- ^ Repartition for jigsaw groups.
	-> [M.Theme] -- ^ Themes.
	-> [[[Participant]]] -- ^ Participants splited per jigsaw groups and per themes.
createParticipantsExpert' [] _ = []
createParticipantsExpert' (p:ps) themes = createParticipantsExpert'' themes p : createParticipantsExpert' ps themes



-- | 'createParticipantsExpert''', split a Jigsaw group between themes.
createParticipantsExpert'' :: [M.Theme] -- ^ Themes.
	-> M.JigsawGroup  -- ^ Participants of one jigsaw group.
	-> [[Participant]] -- ^ Participants splited between themes.
createParticipantsExpert'' [] _ = []
createParticipantsExpert'' themes ps = splitList ps repart
	where
		repart = case repartition2 (length ps) themesInfo of
			Possible list -> list
			NotPossible _ -> []
		themesInfo = map (\t -> (M.nbExpert t, M.upperMargin t, M.lowerMargin t))themes



-- | 'createParticipantsJigsaw', split participants into groups jigsaw.
createParticipantsJigsaw :: [Participant] -- ^ Participants.
	-> [M.Theme] -- ^ Themes.
	-> [M.JigsawGroup] -- ^ Jigsaw groups.
createParticipantsJigsaw participants themes = splitList participants $ possibleToList $ repartitionJigsaw participants themes



-- //////////////////////////////////// RESOURCES ////////////////////////////////////

-- | 'generateResourcesLvl', generate resources level.
generateResourcesLvl :: M.Info -- ^ Pattern information.
	-> Level -- ^ The level resources.
generateResourcesLvl info = ("Resource", map (map (: [])) (createResourcesInitial info ++ createResourcesExpert ps themes ++ createResourcesJigsaw ps themes))
	where
		themes = M.themes info
		ps = participantsList info


-- | 'createResourcesInitial', create resources list for the expert level
createResourcesInitial :: M.Info -- ^ Pattern information.
	-> [[Resource]] -- ^ Resources.
createResourcesInitial info = foldl (\acc (t, gs) -> acc ++ createResourcesInitial' gs (splitList2 (M.resources t) (length gs) (length $ M.resources t)))
	[] $ nbGroupsInitialPerThemes info



-- | 'createResourcesInitial'', create resources list for the initial level.
createResourcesInitial' :: [Int] -- ^ Number of groups for each theme.
	-> [[Resource]] -- ^ List of resources for each theme.
	-> [[Resource]] -- ^ List of resources duplicated for each theme by the number of initial groups for the theme.
createResourcesInitial' [] _ = []
createResourcesInitial' (x:xs) (r:rs) = replicate x r ++ createResourcesInitial' xs rs



-- | 'createResourcesExpert', create resources list for the expert level
createResourcesExpert :: [Participant] -- ^ Participants.
	-> [M.Theme] -- ^ Themes.
	-> [[Resource]] -- ^ Resources.
createResourcesExpert participants themes = createResourcesExpert' themes $ createParticipantsExpert participants themes



-- | 'createResourcesExpert'', create resources for the expert level.
createResourcesExpert' :: [M.Theme] -- ^ Themes.
	-> [M.ExpertGroup] -- ^ Expert groups.
	-> [[Resource]] -- ^ Resources.
createResourcesExpert' [] _ = []
createResourcesExpert' (t:themes) (g:groups) = map (const resourcesTheme) g ++ createResourcesExpert' themes groups
	where
		resourcesTheme = M.resources t



-- | 'createResourcesJigsaw', create resources list for the jigsaw level
createResourcesJigsaw :: [Participant] -- ^ Participants.
	-> [M.Theme] -- ^ Themes.
	-> [[Resource]] -- ^ Resources for the jigsaw level.
createResourcesJigsaw participants themes = replicate (length participants) $ allResources themes



-- //////////////////////////////////// UTIL ////////////////////////////////////

-- | 'repartitionInitial', repartition for initial groups.
repartitionInitial :: M.Info -- ^ Pattern information.
	-> [Possible [Int]] -- ^ Possible repartition.
repartitionInitial info = repartitionInitial' (createParticipantsExpert (participantsList info) $ M.themes info) (M.themes info) (M.nbPPG info) (M.above info) (M.below info)



-- | 'repartitionInitial'', list of size for jigsaw's group.
repartitionInitial' :: [M.ExpertGroup] -- ^ Expert groups.
	-> [M.Theme] -- ^ Themes.
	-> Int -- ^ Number of participants per groups.
	-> Int -- ^ Above margin.
	-> Int -- ^ Below margin.
	-> [Possible [Int]] -- ^ List of size for jigsaw's group.
repartitionInitial' [] _ _ _ _ = []
repartitionInitial' (g:groups) (t:themes) nbPPG above below =
	case repartition (length g) nbPPG above below True of
		NotPossible msg -> NotPossible ("Can't do a good repartition for initial group on the theme " ++ M.name t) : repartitionInitial' groups themes nbPPG above below
		Possible list -> Possible list : repartitionInitial' groups themes nbPPG above below



-- | 'repartitionJigsaw', list of size for jigsaw's group.
repartitionJigsaw :: [Participant] -- ^ Participants.
	-> [M.Theme] -- ^ Themes.
	-> Possible [Int] -- ^ Possible repartition.
repartitionJigsaw participants themes =
	case repartition (length participants) nbPPGJ upperMarginJigsaw lowerMarginJigsaw True of
		NotPossible _ -> NotPossible "Can't do a good repartition for jigsaw groups"
		Possible list -> Possible list
	where
		(nbPPGJ, upperMarginJigsaw, lowerMarginJigsaw) = nbParticipantJigsaw themes



-- | 'allResources', create a list with all the resources.
allResources :: [M.Theme] -- ^ Themes.
	-> [String]	-- ^ All resources.
allResources = foldl (\acc t -> acc ++ M.resources t) []



-- | 'nbParticipantJigsaw', give information on the repartition wanted for jigsaw groups.
nbParticipantJigsaw :: [M.Theme] -- ^ Themes.
	-> (Int, Int, Int) -- ^ (number of participants wanted for a jigsaw group, how many participants additional are tolerated, how many participants less are tolerated)
nbParticipantJigsaw = foldl (\(x,y,z) theme -> (x + M.nbExpert theme, y + M.upperMargin theme, z + M.lowerMargin theme)) (0, 0, 0)



-- | 'nbGroupInitial', number of initial groups.
nbGroupsInitial :: M.Info -- ^ Pattern info.
	-> Int -- ^  Number of initial groups.
nbGroupsInitial info = length $ createParticipantsInitial info



-- | 'nbGroupsInitialPerThemes', number of initial group per themes.
nbGroupsInitialPerThemes :: M.Info -- ^ Pattern info.
	-> [(M.Theme, [Int])] -- ^ (theme, length of groups of the theme)
nbGroupsInitialPerThemes info = foldl (\acc t -> acc ++ [(t, foldl (\acc2 (t2,ps) -> 
	if M.name t2 == M.name t then acc2 ++ [length ps] else acc2) [] $ createParticipantsInitialWithTheme info)]) [] (M.themes info)



-- | 'participantsList', list of participants logins
participantsList :: M.Info -- ^ Pattern information.
	-> [Participant] -- ^ Participants.
participantsList info = participantsLogins $ participantsObjects $ M.objects info



-- | 'groupObjectsList', create the group objects list for .t2
groupObjectsList :: M.Info -- ^ Pattern information.
	-> GroupObjects -- ^ Groups objects for .t2.
groupObjectsList info = groupsInitial ++ groupsExpertJigsaw
	where
		groupsInitial = map (\(n, (t,_)) -> (n, n ++ " sur le thème " ++ M.name t)) $ zip (concat $ createGroupsInitial info) $ createParticipantsInitialWithTheme info
		groupsExpertJigsaw = map (\g -> (g,g)) $ concat (createGroupsExpert (M.themes info) ++ createGroupsJigsaw info)