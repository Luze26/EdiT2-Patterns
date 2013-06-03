import Data.Char( intToDigit )
import Tree
import Util
import qualified EditorPatternInformation as Info



editorColumns :: [String]
editorColumns = ["Topic","Editor1","Section","Editor2"]



tree :: NTree Cell
tree = generate Info.participants Info.nbSec Info.nbSecEditor



-- Generator's entry point
main :: IO()
main = writeFile Info.file $ (showTree editorColumns tree) ++ scriptName ++ (objectList tree) ++ (teacherNotes $ nbLeaf tree)



-- generate, generate the tree
-- @[Participant] -> participants
-- @Int -> number of section
-- @Int -> number of section editor
-- @NTree Cell -> generated tree
generate :: [Participant] -> Int -> Int -> NTree Cell
generate participants nbSec nbSecEditor = Node ("Root","1","null",[]) $ editorLevel [] participants nbSec nbSecEditor 0



-- editorLevel, generate the topic and editor level
-- @[Participant] -> participants already been editor
-- @[Participant] -> participants remaining to be editor
-- @Int -> number of section
-- @Int -> number of section editor
-- @Int -> topic sub-id
-- @NTree Cell -> generated subtree under the root node
editorLevel :: [Participant] -> [Participant] -> Int -> Int -> Int -> [NTree Cell]
editorLevel _ [] _ _ _ = []
editorLevel participants (p:ps) nbSec nbSecEditor i = Node ("Topic", tId, "1", [ "Topic" ++ strI ]) [ Node ("Editor1", eId, tId, [p]) $ sectionLevel subeditor subeditor eId strI nbSec nbSecEditor 0] : editorLevel (p:participants) ps nbSec nbSecEditor (i+1)
	where
		strI = show (i+1)
		tId = '1':strI
		eId = tId ++ "1"
		subeditor = participants ++ ps



-- sectionLevel, generate the section and section editor level
-- @[Participant] -> participants
-- @[Participant] -> participants remaining to be sub-editor
-- @String -> father's numbering
-- @String -> numero of topic
-- @Int -> number of section
-- @Int -> number of section editor
-- @Int -> section sub-id
-- @NTree Cell -> generated subtrees under the editor level
sectionLevel :: [Participant] -> [Participant] -> String -> String -> Int -> Int -> Int -> [NTree Cell]
sectionLevel _ _ _ _ 0 _ _ = []
sectionLevel participants ps eId tId nbSec nbSecEditor i= (Node ("Section", secId, eId, [ "Section" ++ tId ++ strI ]) $ subEditorLevel editors secId 0) : sectionLevel participants ps2 eId tId (nbSec-1) nbSecEditor (i+1)
	where
		strI = show (i+1)
		secId = eId ++ strI
		(editors, ps2) = takeParticipants participants ps nbSecEditor



-- subEditorLevel, generate the sub editor level
-- @[Participant] -> participants
-- @String -> section id
-- @Int -> sub editor id
-- @[NTree Cell] -> 
subEditorLevel :: [Participant] -> String -> Int -> [NTree Cell]
subEditorLevel [] _ _ = []
subEditorLevel (e:editors) secId i = Node ("Editor2", e2Id, secId, [e]) [] : subEditorLevel editors  secId (i+1)
	where
		e2Id = secId ++ show (i+1)



-- takeParticipants, take the number of participants at the beginning of the list and return to the real start if it's the end
-- @[Participant] -> participants
-- @[Participant] -> participants remaining to be sub-editor
-- @Int -> number of section editor
-- @([Participant],[Participant]) -> (participants to be sub-editor, participants remainins)
takeParticipants :: [Participant] -> [Participant] -> Int -> ([Participant], [Participant])
takeParticipants _ ps 0 = ([],ps)
takeParticipants participants [] nbSecEditor = takeParticipants participants participants nbSecEditor
takeParticipants participants (p:ps) nbSecEditor = let (editors, ps2) = takeParticipants participants ps (nbSecEditor-1) in (p:editors,ps2)