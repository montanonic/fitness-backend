module Handler.FriendRequest.Create where

import Import

import Model.Friendship (YourId, TheirId, createFriendRequest)

postCreateFriendRequestR :: YourId -> TheirId -> Handler ()
postCreateFriendRequestR you them = do
    --you <- requireAuthId
    createFR <- liftIO $ createFriendRequest <$> getCurrentTime
    _friendshipId <- runDB $ createFR you them
    -- setNotification them friendshipId -- notification system doesn't exist,
    -- but this is where we'd want to use it
    sendResponse ("Friendship created and request flagged, but no notification\
        \ was sent as this implementation is unfinished." :: Text)
