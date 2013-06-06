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
type ParticipantObjects = [(Login,Firstname,Surname,Email,City,Country)]

-- | 'ResourceObjects'
type ResourceObjects = [(Name,Description,MoodleTableName,MoodleResourceID)]

-- | 'RoleObjects'
type RoleObjects = [(Name,Description)]

-- | 'TeacherNotes'
type TeacherNotes = [String]


-- information
type Name = String
type Description = String
type Login = String
type Firstname = String
type Surname = String
type Email = String
type Country = String
type City = String
type MoodleTableName = String
type MoodleResourceID = String



-- Util functions //////////////////////////////////////////////////////////////////////////////////////

-- | 'participantsLogins', return the list of participants logins.
participantsLogins :: ParticipantObjects -- ^ Participants objects.
	-> [Participant] -- ^ Participants logins.
participantsLogins = map (\(login,_,_,_,_,_) -> login)



-- | 'resourcesNames', returns the list of resources names.
resourcesNames :: ResourceObjects -- ^ Resources objects.
	-> [Resource] -- ^ Resources names.
resourcesNames = map (\(name,_,_,_) -> name)



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



-- 'showObjects', convert pattern objects in a string for .t2
showObjects :: PatternObjects -- ^ Pattern objects.
	-> String -- ^ Pattern objects in .t2 format.
showObjects (a,g,p,r,ro) = "activityObjectsList = " ++ show a ++ "\ngroupObjectsList = " ++ show g ++
	"\nparticipantObjectsList = " ++ show p ++ "\nresourceObjectsList = " ++ show r ++ "\nroleObjectsList = " ++ show ro