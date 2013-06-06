-- | Data strcuture used by Jigsaw.hs.
module Jigsaw.JigsawModel where


import Util.Util ( Participant, Resource, PatternObjects )



-- | Initial Group
type InitialGroup = [Participant]

-- | Expert Group
type ExpertGroup = [Participant]

-- | Jigsaw Group
type JigsawGroup = [Participant]



data Theme =
	-- | 'Theme', a theme in jigsaw's context.
	Theme {
		name :: String, -- ^ Name of the theme.
		resources :: [Resource], -- ^ Resources of the theme.
		nbExpert :: Int, -- ^ Number of expert for the theme needed in a Jigsaw group.
		lowerMargin :: Int, -- ^ Number of additionnal experts allowed in a Jigsaw group.
		upperMargin :: Int, -- ^ Number of less experts allowed in a Jigsaw group.
		nbResources :: Int -- ^ Number of resources of this theme in an initial group.
	} deriving (Read)



data Info =
	-- | 'Info', information on the pattern.
	Info {
		objects :: PatternObjects, -- ^ Pattern objects.
		themes :: [Theme], -- ^ Themes.
		nbPPG :: Int, -- ^ Number of participants per groups.
		above :: Int, -- ^ Above margin.
		below :: Int -- ^ Below margin.
	} deriving (Read)