-- | Module to generate .t2 file for the pattern Editor.
module Editor.Editor (
	run
) where


import Util.Util
import Util.Tree
import Util.TreeGenerator
import Util.KobbeComponents (Participant, ParticipantObjects, Name, Description, participantsLogins, PatternObjectsList)


type Editor = String

type Topic = String

type Section = String

data Info =
	-- | 'Info', contain information needed for the pattern.
	Info {
		infoParticipants :: ParticipantObjects, -- ^ Participants.
		infoNbSec :: Int, -- ^ Number of section per passage.
		infoNbSecEditor :: Int -- ^ Number of editors per section.
	} deriving (Read)



-- | 'run', entry point. Expect a file path in argument pointing to a file containing information needed.
run :: [String] -> IO ()
run [] = putStrLn "Not enough arguments for editor.\nUsage: ediT2-haskell Editor <information file>"
run (fileInfo:_) = do
	text <- readFile fileInfo -- Read the file past in argument
	let (file, info) = readText text (\x -> read x :: Info) -- Extract information from the text. file = output file. info = information.
	let (lvls, objects) = generateLevels info
	if infoNbSecEditor info + 1 <= (length $ infoParticipants info)
	then writeT2 file ["Topic","Editor","Section","Editor2"] (generate lvls) (showObjects objects 1) 2 -- Write a file, with the generated tree and the pattern object.
	else writeT2Err file [NotPossible $ "Not enough participants to have " ++ show (infoNbSecEditor info) ++ " editors per section."]



-- | 'generateLevels', generate levels of the tree.
generateLevels :: Info -- ^ Pattern info.
	-> ([Level], PatternObjectsList) -- ^ Levels and pattern objects.
generateLevels info = ([generateLvlTopics participants, generateLvlEditor participants,
	sectionLvl, generateLvlEditor2 participants nbSecEditor nbSec],
	[topicsObj,infoParticipants info, sectionsObj, infoParticipants info])
	where
		participants = participantsLogins $ infoParticipants info
		nbSec = infoNbSec info
		nbSecEditor = infoNbSecEditor info
		topicsObj = [ ("Topic" ++ show i, "") | i <-[1..length participants] ]
		sectionsObj = map (\x -> (x,"")) $ concat $ concat $ snd sectionLvl
		sectionLvl = generateLvlSection participants nbSec



-- | 'generateLvlTopics', generate the topic level.
generateLvlTopics :: [Participant] -- ^ Participants.
	-> Level -- ^ Topic's level.
generateLvlTopics participants = ("Topic", generateLvlTopics' participants 1)



-- | 'generateLvlTopics'', cf 'generateLvlTopics'.
generateLvlTopics' :: [Participant] -- ^ Participants.
	-> Int -- ^ Numbering of the topic.
	-> [[[Topic]]] -- ^ Topics.
generateLvlTopics' [] _ = []
generateLvlTopics' (_:ts) i = [["Topic" ++ show i]] : generateLvlTopics' ts (i+1)



-- | 'generateLvlEditor', generate the editor's level.
generateLvlEditor :: [Participant] -- ^ Participants.
	-> Level -- ^ Editor's level.
generateLvlEditor participants = ("Editor", generateLvlEditor' participants)



-- | 'generateLvlEditor'', cf 'generateLvlEditor'.
generateLvlEditor' :: [Participant] -- ^ Participants.
	-> [[[Editor]]] -- ^ Editor.
generateLvlEditor' = map (\ p -> [[p]])



-- | 'generateLvlSection', generate the section's level.
generateLvlSection :: [Participant] -- ^ Participants.
	-> Int -- ^ Number of section.
	-> Level -- ^ Section's level.
generateLvlSection participants nbS = ("Section", generateLvlSection' participants nbS 1)



-- | 'generateLvlSection'', cf 'generateLvlSection'.
generateLvlSection' :: [Participant] -- ^ Participants.
	-> Int -- ^ Number of section.
	-> Int -- ^ Numbering of the topic.
	-> [[[Section]]] -- ^ Sections.
generateLvlSection' [] _ _ = []
generateLvlSection' (_:ps) nbS i = generateLvlSection'' nbS i 1 : generateLvlSection' ps nbS (i+1)



-- | 'generateLvlSection''', cf 'generateLvlSection'.
generateLvlSection'' :: Int -- ^ Number of sections.
	-> Int -- ^ Numbering of the topic.
	-> Int -- ^ Section's numbering.
	-> [[Section]] -- ^ Sections.
generateLvlSection'' nbS i i2
	| i2 <= nbS = ["Section" ++ show i ++ show i2] : generateLvlSection'' nbS i (i2+1)
	| otherwise = []



-- | 'generateLvlEditor2', generate level for the sub-editors.
generateLvlEditor2 :: [Participant] -- ^ Participants.
	-> Int -- ^ Number of editors per section.
	-> Int -- ^ Number of sections.
	-> Level -- ^ Second editor level.
generateLvlEditor2 participants nbE nbSec = ("Editor2", generateLvlEditor2' nbE nbSec participants $ take (nbE + 3 * length participants) $ cycle participants)



-- | 'generateLvlEditor2'', cf 'generateLvlEditor2'.
generateLvlEditor2' :: Int -- ^ Number of editors per section.
	-> Int -- ^ Number of editors per section.
	-> [Participant] -- ^ Participants remaining = topics remaining.
	-> [Participant] -- ^ Participants.
	-> [[[Editor]]] -- ^ Sub-editors.
generateLvlEditor2' _ _ [] _ = []
generateLvlEditor2' nbE nbSec (p:ps) participants = map (: []) editors : map (: []) editors2 : generateLvlEditor2' nbE nbSec ps ps3
	where
		(editors, ps2) = takeParticipants participants p nbE
		(editors2, ps3) = takeParticipants ps2 p nbE



-- | 'takeParticipants', take the number of participants wanted from the list, without the participant given. 
takeParticipants :: [Participant] -- ^ Participants.
	-> Participant -- ^ Participant unwanted.
	-> Int -- ^ Number of participants wanted.
	-> ([Participant], [Participant]) -- ^ Participants taken, participants remaining.
takeParticipants ps _ 0 = ([],ps)
takeParticipants (p:ps) p2 nbE
	| p /= p2 = let (editors, ps2) = takeParticipants ps p2 (nbE-1) in (p:editors,ps2)
	| otherwise = takeParticipants ps p2 nbE