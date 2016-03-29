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

{-
    muser <- maybeAuth
    case muser of
        Nothing -> sendResponseStatus status401 ("User must be authenticated to\
            \ query this route" :: Text)
        Just user -> returnJson user
-}

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
    maybe err (sendResponseCreated . BrowseProfileR) muid
    -- the newly created user will still need to log-in following this; we'll
    -- leave this responsibility to the clients for now, as our strategy for
    -- authentication is yet to be solidified.
