module Handler.Friendships where

import Import

import Model.Friendship

-- | This needs to allow for variable sized searches with query params.
getFriendshipsR :: UserId -> Handler Value
getFriendshipsR uid = error "unimplimented getFriendshipsR"
