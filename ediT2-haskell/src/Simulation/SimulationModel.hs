-- | Module for data structure needed by Simulation.
module Simulation.SimulationModel where


import Util.KobbeComponents ( PatternObjects, Role, Resource )



data RoleSimu =
	-- | 'RoleSimu', a role in the simulation pattern.
	RoleSimu {
		name :: Role, -- ^ Name of the role.
		resources :: [Resource], -- ^ Resources linked to the role.
		nbActor :: Int, -- ^ Number of actors for the role in a simulation group.
		above :: Int, -- ^ Above margin.
		below :: Int -- ^ Below margin.
	} deriving (Read)



data Info =
	-- | 'Info', information for the pattern.
	Info {
		objects :: PatternObjects, -- ^ Pattern object.
		roles :: [RoleSimu],	-- ^ Roles.
		resourcesProblem :: [Resource]	-- ^ Resources for the problem.
	} deriving (Read)