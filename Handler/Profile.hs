module Handler.Profile where

import Import

-- | This handler must soon contain (1) Logic that prevents users
-- from seeing most information from profiles that aren't in their friend group,
-- (2) logic that allows users to hide any piece of Profile information from
-- their friends. The privacy system can become even more fine-grained in the
-- future (perhaps as an `operational` monad), but for now, those two levels
-- seem necessary.
getProfileR :: UserId -> Handler Value
getProfileR userId = runDB $ getBy404 (UniqueProfile userId) >>= returnJson

-- | Need to add profile logging using ProfileLog to this handler.
putProfileR :: UserId -> Handler Value
putProfileR userId = do
    profile <- requireJsonBody :: Handler Profile
    runDB $ do
        profileId <- entityKey <$> getBy404 (UniqueProfile userId)
        replace profileId profile
    sendResponse ("UPDATED" :: Text)


{- old handler code, from before the automatic creation of a Profile when
creating a User account:

putProfileR :: UserId -> Handler Value
putProfileR userId = do
    profile <- requireJsonBody :: Handler Profile
    mkey <- runDB $
        (map entityKey) <$> getBy (UniqueProfile userId)
    -- does a profile for this user already exist?
    case mkey of
        -- if not, make one using the request body
        Nothing -> do
            runDB $ insert_ profile
            sendResponseCreated (ProfileR userId)
        -- otherwise, replace the profile with the req body, updating it
        Just profileId -> do
            runDB $ replace profileId profile
            sendResponse ("UPDATED" :: Text)
-}
