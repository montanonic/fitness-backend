module GHCiTesting where

import Import
import Application

import Database.Persist.Sql

import Model
import Model.Persistent
import Model.Friendship
import Model.User

toKey :: Int64 -> BackendKey SqlBackend
toKey = SqlBackendKey

getUser :: Text -> DB (Entity User)
getUser ident = liftM2 fromMaybe (pure undefined) (getBy $ UniqueUser ident)

getUserId :: Text -> DB UserId
getUserId ident = entityKey
    <$> liftM2 fromMaybe (pure undefined) (getBy $ UniqueUser ident)

mkUserId = UserKey . toKey
mkFriendshipId = FriendshipKey . toKey

queryFriendships key1 key2 = do
    let uid1 = mkUserId key1
        uid2 = mkUserId key2
    selectFirst
        [ FriendshipFirstUser <-. [uid1, uid2]
        , FriendshipSecondUser <-. [uid1, uid2] ] []

test :: Handler (Entity Friendship)
test = runDB $ do
    uid1 <- getUserId "1"
    uid2 <- getUserId "2"
    getUniqueFriendship uid2 uid1


createNewUser :: Int -> Handler (Entity User)
createNewUser n = do
    now <- liftIO $ getCurrentTime
    runDB $ do
        muid <- createUserAndProfile now (buildUserAndProfile n)
        case muid of
            Nothing -> error "User could not be created"
            Just uid -> do
                user <- get404 uid
                return (Entity uid user)

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
