module Handler.Profile where

import Import

getProfileR :: UserId -> Handler Value
getProfileR userId = runDB $ getBy404 (UniqueProfile userId) >>= returnJson

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
