module Model.Friendship (
      sendFriendRequest
    , acceptFriendRequest
    ) where

import Import

import Model.Persistent (FriendshipAction(..))

sendFriendRequest :: UserId -> UserId -> UTCTime -> DB FriendshipId
sendFriendRequest self to now = insert $ Friendship self to Nothing Nothing now

acceptFriendRequest :: FriendshipId -> UTCTime -> DB ()
acceptFriendRequest fid now = do
    self <- friendshipSecondUser <$> get404 fid
    update fid [FriendshipBecameFriends =. Just now]
    insert_ $ FriendshipLog fid self AcceptRequest now
