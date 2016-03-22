module Model.User where

import Import
import Control.Error
import Data.Aeson.TH (deriveJSON, defaultOptions)

import Model.Persistent (Gender)

-- | We use this data structure to get from a request body the minimum required
-- information to create a User and their Profile

-- Elaboration: We keep all private information in User, and information allowed
-- to be public in Profile. The reason for using both of them here is that we
-- want Users to start off with a Profile. To create both at once, we use this
-- intermediary data structure to marshall between the front-end interface, and
-- the database design.
data UserAndProfile = UserAndProfile {
      ident :: Text
    , email :: Text
    , password :: Text

    , firstName :: Text
    , lastName :: Text
    , dateOfBirth :: Day
    , gender :: Gender
    } deriving Eq
$(deriveJSON defaultOptions ''UserAndProfile)

createUserAndProfile :: UTCTime -> UserAndProfile -> DB (Maybe UserId)
createUserAndProfile now UserAndProfile{..} = runMaybeT $ do
    let user = User ident email password now now
    let toProfile userId =
            Profile userId firstName lastName dateOfBirth gender now

    uid <- MaybeT $ insertUnique user
    lift $ insert_ (toProfile uid)
    return uid
