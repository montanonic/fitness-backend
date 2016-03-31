module Model.WallMessage where

import Import

-- | Will post to the provided profile. UserId is the author of the post.
createWallPost :: UTCTime -> Text -> UserId -> ProfileId -> DB ()
createWallPost now content you postTo = do
    insert_ $ WallMessage (Just postTo) Nothing you content 0 [] now

createWallComment :: UTCTime -> WallMessageId -> Text -> UserId -> DB ()
createWallComment now wmid content you =
    insert_ $ WallMessage Nothing (Just wmid) you content 0 [] now

likeMessage :: Entity WallMessage -> UserId -> DB ()
likeMessage (Entity wmid WallMessage{..}) you =
    update wmid
        [ WallMessageLikeCount +=. 1
        , WallMessageWhoLiked =. (you:wallMessageWhoLiked) ]

-- | #TODO: Maybe don't use lists for this? Their linear structure will get
-- slow when we're trying to do a search. An unordered set would probably be
-- better here. The problem is: I don't know how to store something like that
-- on the backend...
unlikeMessage :: Entity WallMessage -> UserId -> DB ()
unlikeMessage (Entity wmid WallMessage{..}) you =
    update wmid
        [ WallMessageLikeCount -=. 1
        , WallMessageWhoLiked =. (filter (/= you) wallMessageWhoLiked) ]

-- | Gets all the Wall Posts made by a given user in decreasing order from when
-- they were created.
getWallPosts :: UserId -> DB [Entity WallMessage]
getWallPosts uid = selectList
    [ WallMessageParent ==. Nothing
    , WallMessageAuthor ==. uid ]
    [ Desc WallMessageCreatedAt ]

-- | Similar to 'getWallPosts', but takes a ProfileId instead, since wall posts
-- are all attached to a profile. This is useful when displaying a user's news
-- feed, as we want to include wall posts made to their *profile*, rather than
-- what was authored by them.
getProfilePosts :: ProfileId -> DB [Entity WallMessage]
getProfilePosts pid = selectList
    [ WallMessageProfile ==. (Just pid) ]
    [ Desc WallMessageCreatedAt ]

-- | Gets all of the comments tied to a particular post, in Descending order
-- from when they were created.
getComments :: WallMessageId -> DB [Entity WallMessage]
getComments wmid = selectList
    [ WallMessageParent ==. (Just wmid) ]
    [ Desc WallMessageCreatedAt ]
