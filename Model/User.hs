module Model.User (
      createUserAndProfile
    ) where

import Import
import Control.Monad.Trans.Maybe
import Data.Aeson.TH (deriveJSON, defaultOptions)

import Model.Persistent (Gender)

-- | We use this data structure to get from a request body the minimum required
-- information to create a User and their Profile
data UserAndProfile = UserAndProfile {
      ident :: Text
    , email :: Text
    , password :: Text

    , firstName :: Text
    , lastName :: Text
    , dateOfBirth :: Day
    , gender :: Gender
    }
$(deriveJSON defaultOptions ''UserAndProfile)

createUserAndProfile :: UTCTime -> UserAndProfile -> DB (Maybe UserId)
createUserAndProfile now UserAndProfile{..} = runMaybeT $ do
    let user = User ident email password now
    let toProfile userId =
            Profile userId firstName lastName dateOfBirth gender

    uid <- MaybeT $ insertUnique user
    lift $ insert_ (toProfile uid)
    return uid
