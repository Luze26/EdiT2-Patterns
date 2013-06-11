-- | Module to have types and methods for Kobbe components.
module Util.KobbeComponents where



-- Types ////////////////////////////////////////////////////////////////////////

-- | 'Activity'
type Activity = String

-- | 'Group'
type Group = String

-- | 'Participant'
type Participant = String

-- | 'Role'
type Role = String

-- | 'Resource'
type Resource = String


-- | 'PatternObjects' represents the list of objects for the bases notions
type PatternObjects = (ActivityObjects, GroupObjects, ParticipantObjects, ResourceObjects, RoleObjects)

-- | 'ActivityObjects'
type ActivityObjects = [(Name,Description)]

-- | 'GroupObjects'
type GroupObjects = [(Name,Description)]

-- | 'ParticipantObjects'
type ParticipantObjects = [(Name,Description)]

-- | 'ResourceObjects'
type ResourceObjects = [(Name,Description)]

-- | 'RoleObjects'
type RoleObjects = [(Name,Description)]

-- | 'TeacherNotes'
type TeacherNotes = [String]

type PatternObjectsList = [[(Name,Description)]]

-- information
type Name = String
type Description = String



-- Util functions //////////////////////////////////////////////////////////////////////////////////////

-- | 'participantsLogins', return the list of participants logins.
participantsLogins :: ParticipantObjects -- ^ Participants objects.
	-> [Participant] -- ^ Participants logins.
participantsLogins = map fst



-- | 'resourcesNames', returns the list of resources names.
resourcesNames :: ResourceObjects -- ^ Resources objects.
	-> [Resource] -- ^ Resources names.
resourcesNames = map fst



-- | 'rolesNames', returns the list of roles names.
rolesNames :: RoleObjects -- ^ Roles objects.
	-> [Role] -- ^ Roles names.
rolesNames = map fst



-- | 'activitiesObjects', return the activities objects from a pattern object.
activitiesObjects :: PatternObjects -> ActivityObjects
activitiesObjects (o,_,_,_,_) = o


-- | 'groupsObjects', return the groups objects from a pattern object.
groupsObjects :: PatternObjects ->GroupObjects
groupsObjects (_,o,_,_,_) = o



-- | 'participantsObjects', return the participants objects from a pattern object.
participantsObjects :: PatternObjects -> ParticipantObjects
participantsObjects (_,_,o,_,_) = o



-- | 'resourcesObjects', return the resources objects from a pattern object.
resourcesObjects :: PatternObjects -> ResourceObjects
resourcesObjects (_,_,_,o,_) = o



-- | 'rolesObjects', return the roles objects from a pattern object.
rolesObjects :: PatternObjects -> RoleObjects
rolesObjects (_,_,_,_,o) = o