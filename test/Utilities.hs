module Utilities where

-- inspired heavily by:
-- https://github.com/thoughtbot/carnival/blob/e78b8cdebecfdbdc19627523fe7b85b6ca61d426/test/Factories.hs
-- and by
-- https://robots.thoughtbot.com/on-auth-and-tests-in-yesod

import TestImport
import Model()

buildUser :: UTCTime -> User
buildUser now = User
    { userIdent = "dummy"
    , userFirstName = "J"
    , userLastName = "Doe"
    , userEmail = "jdoe@example.com"
    , userPassword = "1234567"
    , userDateOfBirth = fromGregorian 1994 1 16
    , userCreatedAt = now
    }

createUser :: Text -> DB (Entity User)
createUser ident = do
    now <- liftIO $ getCurrentTime

    insertEntity (buildUser now)
        { userFirstName = "user-" ++ ident
        , userEmail = "user-" ++ ident ++ "@example.com"
        , userIdent = ident
        }

{- Needs fixing
authenticateAs :: Entity User -> YesodExample App ()
authenticateAs (Entity _ u) = do
    root <- appRoot . appSettings <$> getTestYesod

    request $ do
        setMethod "POST"
        addPostParam "ident" $ userIdent u
        setUrl $ root ++ "/auth/page/dummy"
-}
