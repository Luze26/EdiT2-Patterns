module Jigsaw.JigsawConstraints where

import Util.Util
import Util.Constraints
import Util.Tree
import Util.Cell
import Util.KobbeComponents ( Participant, Resource, participantsLogins )
import Util.T2



check :: String -> T2 -> Result
check cmd t2 =
	case cmd of
		"ParticipantPhase" -> CstrBis $ participantsInEachActivity (participantsLogins $ getObject 3 $ t2Objects t2)
		"RessGroupe" -> CstrBis $ ressourceUsed (getObject 4 $ t2Objects t2)
		"JigsawGroups" -> Result $ checkJigsawGroups t2
		_ -> Result (False, ["Unknow jigsaw's constraint: " ++ cmd])



participantsInEachActivity :: [Participant] -> Cstr
participantsInEachActivity participants =
	Cstr { items = map (Identificator "Participant") participants,  command = "under", wher = Label "Activity"}



ressourceUsed :: [(String, String)] -> Cstr
ressourceUsed resObj = Cstr { items = map (Identificator "Resource" . fst) resObj, command = "under", wher = Label "Root"}


checkJigsawGroups t2 = allExperts (participantsByExpertGroups t2) $ groupsToParticipants $
	lookFor (last (subtrees $ t2Tree t2)) (\(Node (x,_,_,_) _) -> x == "Group")

participantsByExpertGroups t2 =  groupsToParticipants $ lookFor ((subtrees $ t2Tree t2) !! 1) (\(Node (x,_,_,_) _) -> x == "Group")

allExperts :: [[Participant]] -> [[Participant]] -> (Bool, [String])
allExperts groupsExpert gsJig = foldl (\(ok1, err1) gJig -> let (ok2, err2) = allExperts' groupsExpert gJig in
	(ok1 && ok2, err1 ++ err2)) (True, []) gsJig



allExperts' :: [[Participant]] -> [Participant] -> (Bool, [String])
allExperts' [] _ = (True, [])
allExperts' ([]:_) _ = (False, [])
allExperts' ((g:gs):gExpert) gJig
	| g `elem` gJig = allExperts' gExpert gJig
	| otherwise = allExperts' (gs:gExpert) gJig



groupsToParticipants :: [NTree Cell] -> [[Participant]]
groupsToParticipants trees = map (concat . map (cellComponents . node) . subtrees) trees

--participantsWithTheme' = map (