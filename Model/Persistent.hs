module Model.Persistent (
    module Model.Persistent) where

{-
Persistent automatically generates Primary Keys (ID's) for each Table row, so
the data below does not have to explicitly encode it. The reference to a
Row's primary key will just be the table-name + 'Id'. So, to reference a
user's unique ID, you'd write `UserId`.

The basic table syntax is a capitalized table-name, followed by indented
entries, each specifying a row Field (the first word), and the Value for
that Field (the second word, which corresponds to a PSQL data type).

For more information on the syntax used here, see:
https://github.com/yesodweb/persistent/wiki/Persistent-entity-syntax
The "Conversion table" from that link contains all of the standard
Persistent data types.

Data types that are not specified in the "Conversion table" are types
created specifically for this application, know as "Sum types". For
example, `Gender` is not a standard Persistent Value. All such data is
created in the Model/Data/ directory. Please consult it for more
information about the usage of a specific data type.

Most tables will contain `createdAt` and `updatedAt` fields. The latter
will be NULL upon creation. Fields in the table which allow UPDATE
actions will be listed in the comment to `updatedAt`.

If you want to search for a particular table, prefix it with # and then
search. For example, `#Friendship` will take you to the Friendship table.

If a table has an updatedAt field, it can be Updated. Otherwise, it allows
only Creation and Reading (querying). If deletion is allowed, it have a
'can delete' comment.

If a table has documentation on it, there will be a 'documented' comment
next to its search tag. Consult the documentation/Model/ directory.

If a field uses a custom data type, the table comment will have a 'data'
entry followed by a list of all custom datatypes used in the table.
-}

import Prelude
import Database.Persist.TH
import Data.Aeson.TH

-- | Currently, a user can send a friend request, accept a request, or defriend,
-- which does not require a request or notification.
data FriendshipAction
    = SendRequest
    | AcceptRequest
    | Defriend
    deriving (Show, Read, Eq)
derivePersistField "FriendshipAction"

--------------------------------------------------------------------------------

-- NOTE: I intend to use only the Male/Female fields for now. The Specify field
-- is included for sake of completeness. I think deferring gender options to
-- a User's Profile might be the best route, while requiring that users
-- select from Male/Female for biological sex identification at the beginning.
--
-- Given that Sex and Gender are not the same, we may want to rephrase this as
-- Sex, given the biological relevance of such information. That said, if this
-- app will be tying a user to their medical records in some way, we'd already
-- have access to biological sex information, so only Gender would not be
-- redundant.

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
    | UpdateEmail
    | UpdateDateOfBirth
    | UpdateGender
    deriving (Show, Read, Eq)
derivePersistField "ProfileUpdate"
