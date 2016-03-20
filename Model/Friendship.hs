module Model.Friendship (
      sendFriendRequest
    , acceptFriendRequest
    , module Model.Friendship
    ) where

import Import

import Model.Persistent (FriendshipAction(..))

import Database.Esqueleto hiding (update, (=.))

sendFriendRequest :: UserId -> UserId -> UTCTime -> DB FriendshipId
sendFriendRequest self to now = insert $ Friendship self to False now

acceptFriendRequest :: FriendshipId -> UTCTime -> DB ()
acceptFriendRequest fid now = do
    self <- friendshipSecondUser <$> get404 fid
    update fid [FriendshipIsActive =. True]
    insert_ $ FriendshipLog fid self AcceptRequest now

acceptFriendRequest' :: FriendshipId -> UTCTime -> DB ()
acceptFriendRequest' fid now = undefined

-- | Determines when two users became friends.
becameFriends :: UserId -> UserId -> UTCTime
becameFriends = error "unimplemented: becameFriends"

testQuery :: FriendshipId -> DB ()
testQuery fid = undefined
