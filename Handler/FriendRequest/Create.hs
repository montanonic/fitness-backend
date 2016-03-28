module Handler.FriendRequest.Create where

import Import

import Model.Friendship (createFriendRequest)

postCreateFriendRequestR :: UserId -> Handler ()
postCreateFriendRequestR them = do
    you <- requireAuthId -- see Note.md
    now <- liftIO getCurrentTime
    runDB $ createFriendRequest now you them
    -- setNotification them friendshipId -- notification system doesn't exist,
    -- but this is where we'd want to use it
    sendResponse ("Friendship created and request flagged, but no notification\
        \ was sent as this implementation is unfinished." :: Text)
