module JigsawPatternInformation where 
 
import JigsawModel 
import Util 
 
themes = [Theme{name="t1", resources = ["r1", "r2"], nbExpert=1, lowerMargin = 0, upperMargin = 0, nbResources = 1},
	Theme{name="tg", resources = ["bhu", "r2"], nbExpert=2, lowerMargin = 1, upperMargin = 0, nbResources = 2},
	Theme{name="te", resources = ["zz", "aa"], nbExpert=2, lowerMargin = 0, upperMargin = 0, nbResources = 2}] 
 
nbPPG :: Int 
nbPPG = 2
 
above :: Int 
above = 0
 
below :: Int 
below = 1
 
file = "../../out/jig.t2" 
 
activityObjectsList :: ActivityObjects 
activityObjectsList = [("Initial","phase de travail initiale"),("Expert","phase de travail expert"),("Jigsaw","phase de travail jigsaw")] 

participantObjectsList :: ParticipantObjects 
participantObjectsList = [("etu1","","","","",""), ("etu2","","","","",""), ("etu3","","","","",""), ("etu4","","","","","")]
 
resourceObjectsList :: ResourceObjects 
resourceObjectsList = [("r1","blablabla","",""),("r2","a","",""),("bhu","fff","",""),("r2","a","",""),("zz","fff","",""),("aa","a","","")]