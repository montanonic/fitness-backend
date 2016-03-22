module Model.Persistent where

import Prelude
import Database.Persist.TH
import Data.Aeson.TH

data FriendshipAction
    = SendRequest
    | CancelRequest
    | AcceptRequest
    | RejectRequest
    | Defriend
    deriving (Show, Read, Eq)
derivePersistField "FriendshipAction"

--------------------------------------------------------------------------------

-- | Binary gender choice + Specify (a more kindly-worded 'Other' field), which
-- takes two Strings. This allows a user to specify their gender and gender
-- pronouns.
-- Client-side and/or server-side validation should be used for the Specify
-- field.
-- It will probably be best to have a list of common options availible. We
-- wouldn't want users abusing this feature, so limiting choices and preventing
-- customizations would safeguard us (and other users) from such abuses.
data Gender
    = Male
    | Female
    -- | Specify           we're going to ignore this field for now
    deriving (Eq, Read, Show)
$(deriveJSON defaultOptions ''Gender)
derivePersistField "Gender"

--------------------------------------------------------------------------------

-- | These are updates a user can perform on their profile information;
-- currently pending Model/CRUD implementations of the Profile and ProfileLog
-- entities. This datatype will be used to log which field the user updated and
-- when.
data ProfileUpdate
    = UpdateFirstName
    | UpdateLastName
    | UpdatePublicEmail
    | UpdateDateOfBirth
    | UpdateGender
    deriving (Show, Read, Eq)
derivePersistField "ProfileUpdate"

--------------------------------------------------------------------------------

data UserUpdate
    = UpdateIdent
    | UpdateEmail
    | UpdatePassword
    deriving (Show, Read, Eq)
derivePersistField "UserUpdate"

--------------------------------------------------------------------------------

data NotificationType
    = FriendRequest
    deriving (Show, Read, Eq)
derivePersistField "NotificationType"
