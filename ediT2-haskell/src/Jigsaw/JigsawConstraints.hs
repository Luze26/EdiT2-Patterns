module Jigsaw.JigsawConstraints where

import Util.Util
import Util.Constraints
import Util.Tree
import Util.Cell
import Util.KobbeComponents ( Participant, participantsLogins )
import Util.T2



check :: String -> T2 -> Result
check cmd t2 =
	case cmd of
		"ParticipantPhase" -> CstrBis $ participantsInEachActivity (participantsLogins $ getObject 3 $ t2Objects t2)
		"RessGroupe" -> CstrBis $ ressourceUsed (getObject 4 $ t2Objects t2)
		"ThemeExpert" -> Result (True, ["TODO"])
		_ -> Result (False, ["Unknow jigsaw's constraint: " ++ cmd])


participantsInEachActivity :: [Participant] -> Cstr
participantsInEachActivity participants = Cstr { items = map (Identificator "Participant") participants,  command = "under", wher = Label "Activity"}



ressourceUsed :: [(String, String)] -> Cstr
ressourceUsed resObj = Cstr { items = map (Identificator "Resource" . fst) resObj, command = "under", wher = Label "Root"}



