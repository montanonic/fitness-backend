module Handler.FriendRequest.Cancel where

import Import

import Model.Friendship (cancelFriendRequest, getUniqueFriendship)

postCancelFriendRequestR :: UserId -> Handler ()
postCancelFriendRequestR them = do
    you <- requireAuthId -- see Note.md
    now <- liftIO getCurrentTime
    runDB $ cancelFriendRequest now you them
