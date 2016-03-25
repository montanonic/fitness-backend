module Handler.UserAndProfileSpec (spec) where

-- | This module also contains code for both User and Profile handlers, as both
-- entities are created at the same time.

import TestImport
import Data.Aeson (encode, decode)
import Yesod.Persist.Core (getBy404, get404)
import Network.Wai.Test (SResponse(..))
import Yesod.Auth (requireAuthId)

import Model.User (UserAndProfile(..), createUserAndProfile)
import Utilities (buildUserAndProfile, authenticateAs, createNewUserAndProfile
    , getJson)


spec :: Spec
spec = withApp $ do

    describe "postUsersR" $ do
        it "creates a user and a profile for that user" $ do
            let identifier = "dumdum"
            let userAndProfile =
                    (buildUserAndProfile 1) { ident = identifier }

            request $ do
                setMethod "POST"
                setUrl UserR
                setRequestBody $ encode userAndProfile

            statusIs 201

            (user, profile) <- runDB $ do
                u <- getBy404 (UniqueUser identifier)
                p <- getBy404 (UniqueProfile (entityKey u))
                return (u,p)

            let dbUserAndProfile User{..} Profile{..} =
                    UserAndProfile userIdent userEmail userPassword
                        profileFirstName profileLastName profileDateOfBirth
                        profileGender
            assertEqual "Input data should be same as that in database"
                (dbUserAndProfile (entityVal user) (entityVal profile))
                (userAndProfile)

    describe "getUserR" $ do
        let msg = "gives a 303 when user is not authenticated/logged-in, "
                ++ "redirecting them to log in"
        it msg $ do
            (user, _) <- createNewUserAndProfile

            getJson UserR

            statusIs 303

        let msg = "gives a 200 when user is authenticated, and the query result"
                ++ " is equal to the user record originally inserted into the DB"
        it msg $ do
            (user, _) <- createNewUserAndProfile

            authenticateAs user

            get UserR

            statusIs 200

            withResponse $ \r -> assertEqual ("user query returns the same user"
                ++ " entity as the one originally inserted with"
                ++ " createNewUserAndProfile")
                (decode $ simpleBody r) (Just user)

{-
    describe "getProfileR" $ do
        it "returns a 200" $ do
            (user, _) <- createNewUserAndProfile

            authenticateAs user

            uid <- requireAuthId

            get (ProfileR uid)

            statusIs 200
-}
