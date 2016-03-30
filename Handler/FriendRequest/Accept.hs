module Handler.FriendRequest.Accept where

import Import

import Model.Friendship (acceptFriendRequest, getUniqueFriendship)

postAcceptFriendRequestR :: UserId -> Handler ()
postAcceptFriendRequestR them = do
    you <- requireAuthId
    now <- liftIO getCurrentTime
    runDB $ acceptFriendRequest now you them
    sendResponse ("You are now friends." :: Text)
