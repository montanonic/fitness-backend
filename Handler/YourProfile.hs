module Handler.YourProfile where

import Import

-- | #NOTE: I'm not sure redirection yields the desired effect, but in case it
-- does, this is a more succinct and clear way to write the code.
getYourProfileR :: Handler Value
getYourProfileR = do
    you <- requireAuthId
    runDB $ getBy404 (UniqueProfile you) >>= returnJson

-- | Need to add profile logging using ProfileLog to this handler.
-- Also need to change this to postUpdateProfileR
postUpdateYourProfileR :: Handler Value
postUpdateYourProfileR = do
    profile <- requireJsonBody :: Handler Profile
    you <- requireAuthId
    runDB $ do
        profileId <- entityKey <$> getBy404 (UniqueProfile you)
        replace profileId profile
    sendResponse ("UPDATED" :: Text)
