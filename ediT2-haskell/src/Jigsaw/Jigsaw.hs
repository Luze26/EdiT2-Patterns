import Data.Char( intToDigit )
import Util.Util
import Util.TreeGenerator
import JigsawModel
import qualified JigsawPatternInformation as Info



-- main, entry point
-- write the generated tree into a file
main :: IO()
main
	| notPossible == [] = writeT2 Info.file ["Activity", "Group", "Participant", "Resource"] (generate generateLevels)
		(Info.activityObjectsList, groupObjectsList, Info.participantObjectsList, Info.resourceObjectsList, [])
	| otherwise = writeT2Err Info.file notPossible



-- notPossible, return the list of not possible actions
-- @[Possible String] -> NotPossible actions
notPossible :: [Possible String]
notPossible = foldl (\acc x -> case x of NotPossible msg -> NotPossible msg : acc; Possible _ -> acc) [] (repartitionJigsaw : repartitionInitial)



-- generateLevels, generatee the levels of the tree
-- @[Level] -> levels
generateLevels :: [Level]
generateLevels = generateActivitiesLvl : generateGroupsLvl : generateParticipantsLvl : generateResourcesLvl : []



-- //////////////////////////////////// ACTIVITIES ////////////////////////////////////

-- generateActivitiesLvl, generate activities level
-- @Level -> activities level
generateActivitiesLvl :: Level
generateActivitiesLvl = ("Activity", [[["Initial"]], [["Expert"]], [["Jigsaw"]]]) 



-- //////////////////////////////////// GROUPS ////////////////////////////////////

-- generateGroupsLvl, generate groups level
-- @Level -> groups level
generateGroupsLvl :: Level
generateGroupsLvl = ("Group", createGroupsInitial : createGroupsExpert : createGroupsJigsaw : [])



-- createGroupsInitial, create initial groups
-- @[[Group]] -> initial groups
createGroupsInitial :: [[Group]]
createGroupsInitial = [["Group " ++ show i] | i <- [1..nbGroupsInitial]]



-- createGroupsExpert, create expert groups
-- @[[Group]] -> expert groups
createGroupsExpert :: [[Group]]
createGroupsExpert = [["Group Expert " ++ (name t)] | t <- Info.themes]



-- createGroupsJigsaw, create jigsaw groups
-- @[[Group]] -> jigsaw groups
createGroupsJigsaw :: [[Group]]
createGroupsJigsaw = [["Group Jigsaw " ++ (show i)] | i <- [1..(length createParticipantsJigsaw)]]



-- //////////////////////////////////// PARTICIPANTS ////////////////////////////////////

generateParticipantsLvl :: Level
generateParticipantsLvl = ("Participant", map (\x -> map (\y -> [y]) x) (createParticipantsInitial ++ createParticipantsExpert ++ createParticipantsJigsaw))



-- createParticipantsInitial, split participants into initial groups
-- @[InitialGroup] -> initial groups
createParticipantsInitial :: [InitialGroup]
createParticipantsInitial = map (\(_,x) -> x) createParticipantsInitialWithTheme




-- createParticipantsInitialWithTheme, split participants into initial groups
-- @[(Theme, InitialGroup)] -> initial groups with their associated theme
createParticipantsInitialWithTheme :: [(Theme, InitialGroup)]
createParticipantsInitialWithTheme = createParticipantsInitial' createParticipantsExpert Info.themes repartitionInitial''



-- createParticipantsInitial', split participants into initial groups from expert groups
-- @[ExpertGroup] -> expert groups
-- @[Theme] -> themes
-- @[(Theme, InitialGroup)] -> initial groups with their associated theme
createParticipantsInitial' :: [ExpertGroup] -> [Theme] -> [[Int]] -> [(Theme, InitialGroup)]
createParticipantsInitial' [] _ _ = []
createParticipantsInitial' (g:groups) (t:themes) (r:rs) = map (\x -> (t,x)) (splitList g r) ++ createParticipantsInitial' groups themes rs



-- createParticipantsExpert, split participants into expert groups
-- Merge participants splited per themes for each jigsaw group, into a list of participant splited only per themes
-- @[ExpertGroup] -> expert groups
createParticipantsExpert :: [ExpertGroup]
createParticipantsExpert = foldl (\acc x -> addAll acc x) (replicate nbThemes []) (createParticipantsExpert' createParticipantsJigsaw)



-- addAll, merge on elements of 2 list
-- @[[Participant]] -> list 1
-- @[[Participant]] -> list 2
-- @[[Participant]] -> merged list
addAll :: [[Participant]] -> [[Participant]] -> [[Participant]]
addAll [] _ = []
addAll (a:acc) (t:ts) = (a ++ t) : addAll acc ts



-- createParticipantsExpert', split jigsaw groups per themes
-- @[JigsawGroup] -> repartition for jigsaw groups
-- @[[[Participant]]] -> participants splited per jigsaw groups and per themes
createParticipantsExpert' :: [JigsawGroup] -> [[[Participant]]]
createParticipantsExpert' [] = []
createParticipantsExpert' (p:ps) = createParticipantsExpert'' Info.themes p : createParticipantsExpert' ps



-- createParticipantsExpert'', split a Jigsaw group between themes
-- @[Theme] -> themes
-- @JigsawGroup-> participant of one jigsaw group
-- @[[Participant]] -> participants splited between themes
createParticipantsExpert'' :: [Theme] -> JigsawGroup -> [[Participant]]
createParticipantsExpert'' [] _ = []
createParticipantsExpert'' themes ps = splitList ps $ repart
	where
		repart = case repartition2 (length ps) themesInfo of
			Possible list -> list
			NotPossible _ -> []
		themesInfo = map (\t -> (nbExpert t, upperMargin t, lowerMargin t)) Info.themes



-- createParticipantsJigsaw, split participants into groups jigsaw
-- @[JigsawGroup] -> jigsaw groups
createParticipantsJigsaw :: [JigsawGroup]
createParticipantsJigsaw = case repartitionJigsaw of
								NotPossible msg -> []
								Possible list ->  splitList participants list



-- //////////////////////////////////// RESOURCES ////////////////////////////////////

-- generateResourcesLvl, generate resources
-- @Level -> the level resources
generateResourcesLvl :: Level
generateResourcesLvl = ("Resource", map (\x -> map (\y -> [y]) x) (createResourcesInitial ++ createResourcesExpert ++ createResourcesJigsaw))



-- createResourcesInitial, create resources list for the expert level
-- @[[Resource]] -> resources
createResourcesInitial :: [[Resource]]
createResourcesInitial = foldl (\acc (t, gs) -> acc ++ (createResourcesInitial' gs $ splitList2 (resources t) (length gs) (nbResources t))) [] nbGroupsInitialPerThemes 



-- createResourcesInitial', create resources list for the expert level
-- @[[Resource]] -> resources
createResourcesInitial' :: [Int] -> [[Resource]] -> [[Resource]]
createResourcesInitial' [] _ = []
createResourcesInitial' (x:xs) (r:rs) = replicate x r ++ createResourcesInitial' xs rs



-- createResourcesExpert, create resources list for the expert level
-- @[[Resource]] -> resources
createResourcesExpert :: [[Resource]]
createResourcesExpert = createResourcesExpert' Info.themes createParticipantsExpert



-- createResourcesExpert', create resources for the expert level
-- @[Theme] -> themes
-- @[ExpertGroup] -> Expert groups
-- @[[Resource]] -> resources
createResourcesExpert' :: [Theme] -> [ExpertGroup] -> [[Resource]]
createResourcesExpert' [] _ = []
createResourcesExpert' (t:themes) (g:groups) = map (\p -> resourcesTheme) g ++ createResourcesExpert' themes groups
	where
		resourcesTheme = resources t


-- createResourcesJigsaw, create resources list for the jigsaw level
-- @[[Resource]] -> resources
createResourcesJigsaw :: [[Resource]]
createResourcesJigsaw = replicate nbParticipants allResources



-- //////////////////////////////////// UTIL ////////////////////////////////////

repartitionInitial :: [Possible [Int]]
repartitionInitial = repartitionInitial' createParticipantsExpert Info.themes



-- repartitionInitial', list of size for jigsaw's group
-- @[Possible [Int]] -> list of size for jigsaw's group
repartitionInitial' :: [ExpertGroup] -> [Theme] -> [Possible [Int]]
repartitionInitial' [] _ = []
repartitionInitial' (g:groups) (t:themes) =
	case repartition (length g) Info.nbPPG Info.above Info.below of
		NotPossible msg -> NotPossible ("Can't do a good reparition for initial group on the theme " ++ (name t)) : repartitionInitial' groups themes
		Possible list -> Possible list : repartitionInitial' groups themes



repartitionInitial'' :: [[Int]]
repartitionInitial'' = repartitionInitial''' repartitionInitial



repartitionInitial''' :: [Possible [Int]] -> [[Int]]
repartitionInitial''' [] = []
repartitionInitial''' (r:rs) =
	case r of
		NotPossible _ -> []
		Possible list -> list : repartitionInitial''' rs



-- repartitionJigsaw, list of size for jigsaw's group
-- @Possible [Int] -> list of size for jigsaw's group
repartitionJigsaw :: Possible [Int]
repartitionJigsaw =
	case repartition (length participants) nbPPGJ upperMarginJigsaw lowerMarginJigsaw of
		NotPossible _ -> NotPossible "Can't do a good reparition for jigsaw groups"
		Possible list -> Possible list
	where
		(nbPPGJ, upperMarginJigsaw, lowerMarginJigsaw) = nbParticipantJigsaw



-- allResources, create a list with all the resources
-- @[String] -> resources
allResources :: [String]
allResources = foldl (\acc t -> acc ++ (resources t)) [] Info.themes



-- nbParticipantJigsaw, give information on the repartition wanted for jigsaw groups
-- @(Int, Int, Int) -> (number of participants wanted for a jigsaw group, how many participants additional are tolerated, 
-- 						how many participants less are tolerated)
nbParticipantJigsaw :: (Int, Int, Int)
nbParticipantJigsaw = foldl (\(x,y,z) theme -> (x + nbExpert theme, y + upperMargin theme, z + lowerMargin theme)) (0, 0, 0) Info.themes



-- nbThemes, number of themes
-- @Int -> number of themes
nbThemes :: Int
nbThemes = length Info.themes



-- nbGroupInitial, number of initial group
-- @Int -> number of initial group
nbGroupsInitial :: Int
nbGroupsInitial = length createParticipantsInitial



-- nbGroupsInitialPerThemes, number of initial group per themes
-- @[(Theme, [Int])] -> (theme, length of groups of the theme)
nbGroupsInitialPerThemes :: [(Theme, [Int])]
nbGroupsInitialPerThemes = foldl (\acc t -> acc ++ [(t, foldl (\acc2 (t2,ps) -> if (name t2 == name t) then acc2 ++ [length ps] else acc2) [] createParticipantsInitialWithTheme)]) [] Info.themes



-- nbParticipants, number of participants
-- @Int -> number of participants
nbParticipants :: Int
nbParticipants = length participants



-- participants, list of participants logins
-- @[Participant] -> list of participants logins
participants :: [Participant]
participants = participantsLogins Info.participantObjectsList



-- groupObjectsList, create the group objects list for .t2
-- @GroupObjects
groupObjectsList :: GroupObjects
groupObjectsList = groupsInitial ++ groupsExpertJigsaw
	where
		groupsInitial = map (\(n, (t,_)) -> (n, n ++ " sur le thème " ++ (name t))) $ zip (concat createGroupsInitial) createParticipantsInitialWithTheme
		groupsExpertJigsaw = map (\g -> (g,g)) $ concat (createGroupsExpert ++ createGroupsJigsaw)