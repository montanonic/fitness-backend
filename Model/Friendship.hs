module Model.Friendship (
      sendFriendRequest
    , acceptFriendRequest
    ) where

import Import

sendFriendRequest :: UserId -> UserId -> UTCTime -> Friendship
sendFriendRequest self to now = Friendship self to Nothing Nothing now

acceptFriendRequest :: Friendship -> UTCTime -> Friendship
acceptFriendRequest f@Friendship{..} now =
    f { friendshipBecameFriends = Just now }

defriend :: Friendship
defriend = undefined
