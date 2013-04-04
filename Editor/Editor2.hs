import System.Environment( getArgs )
import Data.Char( intToDigit )
import Util
import Tree



-- Generate, entry point. Create the participant's list
main :: IO()
main = do
	args <- getArgs
	start (args ++ [createList 20])



-- start, parse arguments and write generated tree in a file
-- @[String] -> args
start :: [String] -> IO()
start (file:nbSec:e:ps:_) = writeFile file ("[\"Topic\",\"Editor\",\"Section\",\"Editor\"]\n\nscript=" ++ (printTree True "" $ generate (read ps::[String]) (readInt nbSec) $ readInt e))



-- generate, generate the tree
-- @[String] -> participants
-- @Int -> number of section
-- @Int -> number of section editor
-- @NTree Cell -> generated tree
generate :: [String] -> Int -> Int -> NTree Cell
generate participants nbSec nbSecEditor = Node ("Root","1","null",[]) $ editorLevel [] participants nbSec nbSecEditor 0



-- editorLevel, generate the topic and editor level
-- @[String] -> participants already been editor
-- @[String] -> participants remaining to be editor
-- @Int -> number of section
-- @Int -> number of section editor
-- @Int -> topic sub-id
-- @NTree Cell -> generated subtree under the root node
editorLevel :: [String] -> [String] -> Int -> Int -> Int -> [NTree Cell]
editorLevel _ [] _ _ _ = []
editorLevel participants (p:ps) nbSec nbSecEditor i = Node ("Topic", tId, "1", [ "Topic" ++ strI ]) [ Node ("Editor", eId, tId, [p]) $ sectionLevel subeditor subeditor eId strI nbSec nbSecEditor 0] : editorLevel (p:participants) ps nbSec nbSecEditor (i+1)
	where
		strI = show (i+1)
		tId = '1':strI
		eId = tId ++ "1"
		subeditor = participants ++ ps


-- sectionLevel, generate the section and section editor level
-- @[String] -> participants
-- @[String] -> participants remaining to be sub-editor
-- @String -> father's numbering
-- @String -> numero of topic
-- @Int -> number of section
-- @Int -> number of section editor
-- @Int -> section sub-id
-- @NTree Cell -> generated subtrees under the editor level
sectionLevel :: [String] -> [String] -> String -> String -> Int -> Int -> Int -> [NTree Cell]
sectionLevel _ _ _ _ 0 _ _ = []
sectionLevel participants ps eId tId nbSec nbSecEditor i= (Node ("Section", secId, eId, [ "Section" ++ tId ++ strI ]) $ subEditorLevel editors secId 0) : sectionLevel participants ps2 eId tId (nbSec-1) nbSecEditor (i+1)
	where
		strI = show (i+1)
		secId = eId ++ strI
		(editors, ps2) = takeParticipants participants ps nbSecEditor


subEditorLevel :: [String] -> String -> Int -> [NTree Cell]
subEditorLevel [] _ _ = []
subEditorLevel (e:editors) secId i = Node ("Editor", e2Id, secId, [e]) [] : subEditorLevel editors  secId (i+1)
	where
		e2Id = secId ++ show (i+1)

-- takeParticipants, take the number of participants at the beginning of the list and return to the real start if it's the end
-- @[String] -> participants
-- @[String] -> participants remaining to be sub-editor
-- @Int -> number of section editor
-- @([String],[String]) -> (participants to be sub-editor, participants remainins)
takeParticipants :: [String] -> [String] -> Int -> ([String], [String])
takeParticipants _ ps 0 = ([],ps)
takeParticipants participants [] nbSecEditor = takeParticipants participants participants nbSecEditor
takeParticipants participants (p:ps) nbSecEditor = let (editors, ps2) = takeParticipants participants ps (nbSecEditor-1) in (p:editors,ps2)
