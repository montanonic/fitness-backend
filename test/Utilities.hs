module Utilities
    ( module Utilities
    ) where

-- inspired heavily by:
-- https://github.com/thoughtbot/carnival/blob/e78b8cdebecfdbdc19627523fe7b85b6ca61d426/test/Factories.hs
-- and by
-- https://robots.thoughtbot.com/on-auth-and-tests-in-yesod

import TestImport
import Yesod.Persist.Core (getBy404, get404)
import Yesod.Core (Yesod)
import Yesod.Core.Handler (RedirectUrl)
import Yesod.Auth (Route( PluginR, LogoutR ))

import Model.Persistent (Gender(..))
import Model.User (UserAndProfile(..), createUserAndProfile)
import Model.Friendship (createFriendRequest, acceptFriendRequest)

--
-- Generic
--

getJson :: (Yesod site, RedirectUrl site url) => url -> YesodExample site ()
getJson url = request $ do
    setMethod "GET"
    addGetParam "Accept" "application/json"
    setUrl url

--
-- ## User and Profile
--
createNewUserAndProfileEnt :: Int -> YesodExample App (Entity User)
createNewUserAndProfileEnt n = do
    now <- liftIO $ getCurrentTime
    runDB $ do
        muid <- createUserAndProfile now (buildUserAndProfile n)
        case muid of
            Nothing -> error "User could not be created"
            Just uid -> do
                user <- get404 uid
                return (Entity uid user)

createNewUserAndProfile :: Int -> YesodExample App (UserId, User)
createNewUserAndProfile n = (\x -> (entityKey x, entityVal x)) <$>
    createNewUserAndProfileEnt n

buildUserAndProfile :: Int -> UserAndProfile
buildUserAndProfile n = UserAndProfile
    { ident = (tshow n)
    , email = "jdoe" ++ (tshow n) ++ "@example.com"
    , password = "1234567"

    , firstName = "J" ++ (tshow n)
    , lastName = "Doe"
    , dateOfBirth = fromGregorian 1994 1 16
    , gender = Female
    }

createUserEnt :: Int -> YesodExample App (Entity User)
createUserEnt n = do
    (uid, user) <- createNewUserAndProfile n
    return $ Entity uid user

authenticateAs :: User -> YesodExample App ()
authenticateAs u = do
    request $ do
        setMethod "POST"
        addPostParam "ident" $ userIdent u
        setUrl (AuthR (PluginR "dummy" []))

logout :: YesodExample App ()
logout = do
    post (AuthR LogoutR)

createTwoUsers :: YesodExample App (Entity User, Entity User)
createTwoUsers = do
    e1@(Entity yourId you) <- createNewUserAndProfileEnt 1
    e2@(Entity theirId them) <- createNewUserAndProfileEnt 2
    return (e1, e2)

--
-- ## Friendship
--

makeFriends :: Entity User -> Entity User -> YesodExample App ()
makeFriends (Entity yourId you) (Entity theirId them) = do
    now <- liftIO getCurrentTime
    authenticateAs you
    runDB $ createFriendRequest now yourId theirId
    authenticateAs them
    -- reverse the order of you and them, as this second function is designed to
    -- be called by the receiver of the request
    runDB $ acceptFriendRequest now theirId yourId
