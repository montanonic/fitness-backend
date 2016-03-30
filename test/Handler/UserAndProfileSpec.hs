module Handler.UserAndProfileSpec (spec) where

-- | This module also contains code for both User and Profile handlers, as both
-- entities are created at the same time.

import TestImport

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

        it "gives a 500 if creating a user with a non-unique indent" $ do
            let identifier = "dumdum"
            let userAndProfile1 =
                    (buildUserAndProfile 1) { ident = identifier }

            let userAndProfile2 =
                    (buildUserAndProfile 1) { ident = identifier }

            request $ do
                setMethod "POST"
                setUrl UserR
                setRequestBody $ encode userAndProfile1

            statusIs 201

            request $ do
                setMethod "POST"
                setUrl UserR
                setRequestBody $ encode userAndProfile2

            statusIs 500


    describe "getUserR" $ do

        it ("gives a 401 when user is not authenticated/logged-in, returning"
                ++ "JSON with a link showing where to log in") $ do
            (_, user) <- createNewUserAndProfile 1

            getJson UserR
            statusIs 401

        let msg = "gives a 200 when user is authenticated, and the query result"
                ++ " is equal to the user record originally inserted into the DB"
        it msg $ do
            (_, user) <- createNewUserAndProfile 1

            authenticateAs user

            getJson UserR

            statusIs 200

            withResponse $ \r -> assertEqual ("user query returns the same user"
                ++ " entity as the one originally inserted with"
                ++ " createNewUserAndProfile")
                (decode $ simpleBody r) (Just user)


    describe "getYourProfileR" $ do
        it "returns a 200" $ do
            (_, user) <- createNewUserAndProfile 1

            authenticateAs user

            getJson YourProfileR

            statusIs 200
