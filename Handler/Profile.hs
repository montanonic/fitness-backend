module Handler.Profile where

import Import

getProfileR :: UserId -> Handler Value
getProfileR userId = runDB $ getBy404 (UniqueProfile userId) >>= returnJson

putProfileR :: UserId -> Handler Value
putProfileR userId = do
    profile <- requireJsonBody :: Handler Profile
    runDB $ do
        key <- entityKey <$> getBy404 (UniqueProfile userId)
        replace key profile
    sendResponseStatus status200 ("CREATED/UPDATED" :: Text)
