module Jigsaw.JigsawConstraints where

import Util.Util
import Util.Constraints
import Util.Tree
import Util.Cell
import Util.KobbeComponents ( Participant, participantsLogins )
import Util.T2



check :: String -> T2 -> Result
check cmd t2
	| cmd == "c1" = CstrBis $ participantsInEachActivity (participantsLogins $ getObject 3 $ t2Objects t2)
	| otherwise = Result (False, ["Unknow jigsaw's constraint: " ++ cmd])


participantsInEachActivity :: [Participant] -> Cstr
participantsInEachActivity participants = Cstr { items = map (Identificator "Participant") participants,  command = "under", wher = Label "Activity"}