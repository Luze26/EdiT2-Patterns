module RT_Info
(
	Information(..),
	Passage,
	readInfo
) where
	
import Util

type Passage = String

data Information = Information {
				participants :: [Participant],
				passages :: [String],
				nbParticipantsPerGroup :: Int
			}  deriving (Show, Read)

readInfo :: String -> Information
readInfo info = read info
