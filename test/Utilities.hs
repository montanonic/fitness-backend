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
import Yesod.Auth (Route( PluginR ))

import Model.Persistent (Gender(..))
import Model.User (UserAndProfile(..), createUserAndProfile)

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
createNewUserAndProfile = do
    now <- liftIO $ getCurrentTime
    runDB $ do
        muid <- createUserAndProfile now (buildUserAndProfile 1)
        case muid of
            Nothing -> error "User could not be created"
            Just uid -> do
                user <- get404 uid
                return (user, uid)

buildUserAndProfile :: Int -> UserAndProfile
buildUserAndProfile n = UserAndProfile
    { ident = "dummy" ++ (tshow n)
    , email = "jdoe" ++ (tshow n) ++ "@example.com"
    , password = "1234567"

    , firstName = "J" ++ (tshow n)
    , lastName = "Doe"
    , dateOfBirth = fromGregorian 1994 1 16
    , gender = Female
    }

authenticateAs :: User -> YesodExample App ()
authenticateAs u = do
    request $ do
        setMethod "POST"
        addPostParam "ident" $ userIdent u
        setUrl (AuthR (PluginR "dummy" []))
