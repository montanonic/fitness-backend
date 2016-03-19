module Handler.User where

import Import
import Data.Aeson.TH (deriveJSON, defaultOptions)

import Model.Persistent (Gender)

--
-- ## /user
--

-- | Return the logged-in user's own User entity.
getUserR :: Handler Value
getUserR = requireAuth >>= returnJson

-- | Create a User. Currently the client has to provide the User's unique ident.
-- This will definitely be subject to change later on, and so it is priority to
-- create a test that will fail when other authentication options are used aside
-- from dummyAuth in order to ensure that this code gets updated.
--
-- There is a vulnerability here, in that we do not require authentication to
-- create a user. This means that a client can freely flood the database with
-- new users at the sole cost of a request. The next stage of this handler must
-- be to ensure that a client's user is authenticated before proceeding with
-- this request.
postUserR :: Handler ()
postUserR = do
    userAndProfile <- requireJsonBody :: Handler CreateUserAndProfile
    now <- liftIO $ getCurrentTime

    let toUser CreateUserAndProfile{..} = User ident email password now
    let toProfile userId CreateUserAndProfile{..} =
            Profile userId firstName lastName dateOfBirth gender

    uid <- runDB $ do
        muid <- insertUnique (toUser userAndProfile)
        case muid of
            Nothing -> sendResponseStatus status500 ("Email is already taken.\
                \ Alternatively, since we create \"ident\"s manually in\
                \ development, the \"ident\" given may not be unique.\
                \ If this issue persists once we have a proper authentication\
                \ mechanism, please tell me." :: Text)
            Just uid -> do
                insert_ (toProfile uid userAndProfile)
                -- The resource we link to is for the user's profile, as the
                -- user resource is not intended to be used for anything other
                -- than identifiers and rights management.
                return uid
    sendResponseCreated (ProfileR uid)
    -- the newly created user will still need to log-in following this; we'll
    -- leave this responsibility to the clients for now, as our strategy for
    -- authentication is yet to be solidified.
-- Design Notes for postUserR: We keep all private information in User, and
-- information allowed to be public in Profile. Yet the user-facing interface
-- should require data that will be used in both entities, namely, those fields
-- from the CreateUserAndProfile data structure above. To create a new User, we
-- want to require all of those pieces of information, but the User entity
-- itself technically only requires email and password fields to create. Hence,
-- we use the intermediary data structure to marshall between the information
-- our front-ends ask for, and the database design on the backend.

-- | We're going to want to make sure any actual changes get logged, so the code
-- for this absolutely must be bulked up. Furthermore, because logging can be
-- abused to dump trash data by repeatedly sending requests which alter code
-- (from a malicious client), we must ensure that we have some protocol to guard
-- against that, timeouts being the most obvious way, but also tracking repeat
-- offenders. This may warrant its own table in the future, as more types of
-- threats are uncovered, and we seek to keep track of potentially dangerous
-- or malicious "users".
putUserR :: Handler ()
putUserR = do
    userId <- requireAuthId
    user <- requireJsonBody :: Handler User
    runDB $ replace userId user
    sendResponse ("UPDATED" :: Text)


--
-- # postUserR helpers
--

-- | We use this data structure to get from a request body the minimum required
-- information to create a User and their Profile
data CreateUserAndProfile = CreateUserAndProfile {
      ident :: Text
    , email :: Text
    , password :: Text

    , firstName :: Text
    , lastName :: Text
    , dateOfBirth :: Day
    , gender :: Gender
    }
$(deriveJSON defaultOptions ''CreateUserAndProfile)
