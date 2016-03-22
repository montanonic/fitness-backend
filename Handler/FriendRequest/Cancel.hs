module Handler.FriendRequest.Cancel where

import Import

import Model.Friendship (YourId, TheirId, cancelFriendRequest,
    getUniqueFriendship)

postCancelFriendRequestR :: YourId -> TheirId -> Handler ()
postCancelFriendRequestR you them = do
    -- you <- requireAuthId
    cancel <- liftIO $ cancelFriendRequest <$> getCurrentTime
    runDB $ do
        mfriendshipEnt <- getUniqueFriendship you them
        case mfriendshipEnt of
            Nothing -> sendResponseStatus status500
                            ("No request was sent" :: Text)
            Just friendshipEnt -> void $ cancel you friendshipEnt
