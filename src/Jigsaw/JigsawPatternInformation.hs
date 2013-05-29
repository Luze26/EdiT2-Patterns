module JigsawPatternInformation where 
 
import JigsawModel 
import Util 
 
themes = [Theme{name="t1", resources = ["r1", "r2"], nbExpert=1, lowerMargin = 0, upperMargin = 0},Theme{name="tg", resources = ["bhu", "r2"], nbExpert=1, lowerMargin = 0, upperMargin = 0}] 
 
nbPPG :: Int 
nbPPG = 1
 
above :: Int 
above = 0
 
below :: Int 
below = 0
 
dispatchResources = True 
 
file = "../../out/jig.t2" 
 
activityObjectsList :: ActivityObjects 
activityObjectsList = [("Initial","phase de travail initiale"),("Expert","phase de travail expert"),("Jigsaw","phase de travail jigsaw")] 
 
groupObjectsList :: GroupObjects 
groupObjectsList = [("groupe initiaux","les groupes de la premiÃ¨re activitÃ©(pouvant Ãªtre individuel, correspondant Ã  la phase d'Ã©tude d'un des thÃ¨mes"),("Groupe Expert","groupes de la deuxiÃ¨me activitÃ©; rÃ©unissant les groupes initiaux ayant travaillÃ©s sur le mÃªme thÃ¨me"),("Groupe Jigsaw","groupes de la derniÃ¨re activitÃ©, groupes mÃ©langeant des experts couvrants tous les thÃ¨mes dans le but de rÃ©soudre le problÃ¨me final")] 
 
participantObjectsList :: ParticipantObjects 
participantObjectsList = [("etu1","","","","",""), ("etu2","","","","",""), ("etu3","","","","",""), ("etu4","","","","","")]
 
resourceObjectsList :: ResourceObjects 
resourceObjectsList = [("r1","blablabla","",""),("r2","a","",""),("bhu","fff","",""),("r2","a","","")]
 
teacherNotes :: TeacherNotes 
teacherNotes = ["qq","","","",""]