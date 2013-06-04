module SimulationModel where

import Util.Util ( PatternObjects, Role, Resource )



data Roles =
	Roles {
		name :: Role,
		resources :: [Resource]
		nbActor :: Int,
		above :: Int,
		below :: Int
	}



data Info =
	Info {
		objects :: PatternObjects,
		roles :: Roles
	}