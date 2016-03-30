module Handler.Defriend where

import Import

import Model.Friendship

postDefriendR :: UserId -> Handler ()
postDefriendR them = do
    you <- requireAuthId
    now <- liftIO getCurrentTime
    runDB $ defriend now you them
    sendResponse ("You are no longer friends" :: Text)
