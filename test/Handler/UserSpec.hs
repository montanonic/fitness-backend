module Handler.UserSpec (spec) where

import TestImport

import Utilities (buildUser)
import Data.Aeson (encode)

spec :: Spec
spec = withApp $ do

    -- this must be updated to fall in line with the new UserR handlers.
    describe "postUsersR" $ do
        it "creates a user" $ do
            now <- liftIO $ getCurrentTime
            let identifier = "testIdent"
            let user = (buildUser now) { userIdent = identifier }

            request $ do
                setMethod "POST"
                setUrl UsersR
                setRequestBody $ encode user

            statusIs 201

            muser <- runDB $ getBy (UniqueUser identifier)
            assertEqual "Input User data should be same as that in database"
                (entityVal <$> muser)
                (Just user)
{-
    describe "getUserR" $ do
        it "

        it "queries the database for the current user's entity" $ do
            -- dummy login
            request $ do
                setMethod "POST"
                setUrl (AuthR (PluginR "dummy" []))


-}
