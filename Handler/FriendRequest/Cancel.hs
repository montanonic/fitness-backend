module Handler.FriendRequest.Cancel where

import Import

import Model.Friendship (cancelFriendRequest, getUniqueFriendship)

postCancelFriendRequestR :: UserId -> Handler ()
postCancelFriendRequestR them = do
    you <- requireAuthId -- see Note.md
    cancel <- liftIO $ cancelFriendRequest <$> getCurrentTime
    runDB $ do
        mfriendshipEnt <- getUniqueFriendship you them
        case mfriendshipEnt of
            Nothing -> sendResponseStatus status500
                            ("No friend request was sent" :: Text)
            Just friendshipEnt -> void $ cancel you friendshipEnt
