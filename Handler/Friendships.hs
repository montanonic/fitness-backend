module Handler.Friendships where

import Import

import Model.Friendship (listFriendIds)

-- | This needs to allow for variable sized searches with query params.
getFriendshipsR :: UserId -> Handler Value
getFriendshipsR uid = runDB $ listFriendIds uid >>= returnJson
