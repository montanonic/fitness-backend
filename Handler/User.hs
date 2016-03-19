module Handler.User where

import Import
import Text.RawString.QQ

import Model.User (UserAndProfile, createUserAndProfile)

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
    userAndProfile <- requireJsonBody :: Handler UserAndProfile
    now <- liftIO $ getCurrentTime

    muid <- runDB $ createUserAndProfile now userAndProfile

    let errMsg = asSingleLine [r|Email is already taken. Alternatively, since we
create "ident"s manually in development, the "ident" given may not be unique. If
the Unique constraint violation is not due to email once we have a proper
authentication mechanism, please tell me.|]

    let err = sendResponseStatus status500 errMsg

    -- The resource we link to is for the user's profile, as the
    -- user resource is not intended to be used for anything other
    -- than identifiers and rights management.
    maybe err (sendResponseCreated . ProfileR) muid
    -- the newly created user will still need to log-in following this; we'll
    -- leave this responsibility to the clients for now, as our strategy for
    -- authentication is yet to be solidified.


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
