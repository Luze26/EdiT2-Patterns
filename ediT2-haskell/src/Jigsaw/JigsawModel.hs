module JigsawModel where

import Util

-- Initial Group
type InitialGroup = [Participant]

-- Expert Group
type ExpertGroup = [Participant]

-- Jigsaw Group
type JigsawGroup = [Participant]

-- Theme
data Theme =
	Theme {
		name :: String,				-- name of the theme
		resources :: [Resource],	-- resources of the theme
		nbExpert :: Int,			-- number of expert for the theme needed in a Jigsaw group
		lowerMargin :: Int,			-- number of additionnal experts allowed in a Jigsaw group
		upperMargin :: Int,			-- number of less experts allowed in a Jigsaw group
		nbResources :: Int			-- number of resources of this theme in an initial group
	} deriving (Show)