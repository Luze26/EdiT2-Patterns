module RT_Generate where

import Util
import RT_Info( Information(..), Passage )
import Data.Char( intToDigit )

{-===========================================================================-}
{-============================== MAIN FUNCTION ==============================-}
generate :: Information -> String
generate info = header ++ (genPassages info) ++ "]"

header :: String
header = "[\"Passage\",\"Group\",\"Participant\"]\n\nscript = Node (\"Root\",\"1\",\"null\",[]) [\n"

genPassages :: Information -> String
genPassages Information { participants = p, passages = pa, nbParticipantsParGroupe = nbPPG } = genPassages' pa 1 $ createGroups p nbPPG

genPassages' :: [String] -> Int -> [[Participant]] -> String
genPassages' [] _ _ = ""
genPassages' _ _ [] = ""
genPassages' (pa:passages) i (p:participants) = "\tNode (\"Passage\",\"1" ++ [intToDigit i] ++ "\",\"1\",[\"" ++ pa ++ "\"]) [\n" ++ (genPassages' passages (i+1) participants)

createGroups :: [Participant] -> Int -> [[Participant]]
createGroups [] _ = []
createGroups participants nbPPG = first : (createGroups rest nbPPG)
	where
		(first, rest) = splitAt nbPPG participants