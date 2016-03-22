module Model.Friendship (
      YourId
    , TheirId
    , FriendshipException(..)
    , createFriendRequest
    , cancelFriendRequest
    , acceptFriendRequest
    , getUniqueFriendship
    , getUniqueFriendshipId
    , becameFriends
    , lastBecameFriends
    , listFriendIds
    ) where

import Import

import Model.Persistent (FriendshipAction(..))

data FriendshipException
    = AlreadyFriends
    | NotCurrentlyFriends
    | NeverFriends
    | CannotBeFriendsWithSelf
    | YouDidNotSendRequest
instance Show FriendshipException where
    show AlreadyFriends = "You are already friends."
    show NotCurrentlyFriends = "You are not currently friends with this person."
    show NeverFriends = "These users have never been friends."
    show CannotBeFriendsWithSelf = "You can't be friends with yourself. (At\
            \ least not in this app.)"
    show YouDidNotSendRequest = "You can't cancel a request you didn't send."
instance Exception FriendshipException


--
-- ## Basics
--

-- | This is from the perspective of the client calling the function. These
-- types simply make it clearer which Id should go where.
type YourId = UserId
type TheirId = UserId

-- | Helper combinator function that ensures a specific exception is thrown if a
-- friendship db action is used with a user making themselves the sender and
-- recipient.
cannotBeFriendswithSelf :: Eq a => (a -> a -> DB b) -> a -> a -> DB b
cannotBeFriendswithSelf f uid1 uid2
    | uid1 == uid2 = throwM CannotBeFriendsWithSelf
    | otherwise = f uid1 uid2

-- | This function, along with its
getUniqueFriendship :: UserId -> UserId -> DB (Maybe (Entity Friendship))
getUniqueFriendship = cannotBeFriendswithSelf f where
    f uid1 uid2 = selectFirst
        [ FriendshipFirstUser <-. [uid1, uid2]
        , FriendshipSecondUser <-. [uid1, uid2] ] []

getUniqueFriendshipId :: UserId -> UserId -> DB (Maybe FriendshipId)
getUniqueFriendshipId uid1 uid2 =
    (map entityKey) <$> getUniqueFriendship uid1 uid2


--
-- ## Friendship creation / Friend requests.
--

-- | If already friends, throws an error. Needs notification system.
createFriendRequest :: UTCTime -> YourId -> TheirId -> DB FriendshipId
createFriendRequest now' = cannotBeFriendswithSelf (f now') where
    f now you them = do
        mfid <- insertUnique $ Friendship you them False False now
        maybe' mfid (throwM AlreadyFriends)
            (\fid -> do
                insert_ $ FriendshipLog fid you SendRequest now
                return fid)

-- | Needs logic to remove notification from the person you sent the request to.
cancelFriendRequest :: UTCTime -> YourId -> Entity Friendship -> DB FriendshipId
cancelFriendRequest now you (Entity fid fship)
    | friendshipFirstUser fship /= you = throwM YouDidNotSendRequest
    | friendshipIsActive fship == True = throwM AlreadyFriends
    | otherwise = do
        update fid [ FriendshipIsJustRequest =. False ]
        insert_ $ FriendshipLog fid you CancelRequest now
        return fid

{-
-- | Nothing if already friends.
createFriendRequest :: YourId -> TheirId -> UTCTime -> DB (Maybe FriendshipId)
createFriendRequest self to now = runMaybeT $ do
    fid <- MaybeT $ insertUnique $ Friendship self to False now
    lift $ insert_ $ FriendshipLog fid self SendRequest now
    return fid
-}

acceptFriendRequest :: FriendshipId -> UTCTime -> DB ()
acceptFriendRequest fid now = do
    friendship <- get404 fid
    if friendshipIsActive friendship == False
        then do
            let you = friendshipSecondUser friendship
            update fid [ FriendshipIsActive =. True ]
            insert_ $ FriendshipLog fid you AcceptRequest now
        else throwM AlreadyFriends

-- | Needs to (1) remove notification that you have a friend request, and (2)
-- not change the fact that the friend request is pending for another set
-- period of time, to prevent the user from re-sending, and from knowing the
-- request was rejected.
ignoreFriendRequest = undefined


--
-- ## Friendship Querying
--

-- | Shows each time a pair of Users became friends. Null represents them
-- never having been friends. Sorted from earliest to latest.
becameFriends :: UserId -> UserId -> DB [UTCTime]
becameFriends uid1 uid2 = do
    mfid <- getUniqueFriendshipId uid1 uid2
    maybe' mfid (throwM NeverFriends) f
      where
        f fid = (map (friendshipLogCreatedAt . entityVal)) <$> selectList
            [ FriendshipLogFriendship ==. fid
            , FriendshipLogAction ==. AcceptRequest ]
            [ Asc FriendshipLogCreatedAt ]

-- | Same as becameFriends, but sorted from latest to earliest. These functions
-- are identical when used on friends who've never defriended one-another, or
-- users who weren't friends.
lastBecameFriends :: UserId -> UserId -> DB [UTCTime]
lastBecameFriends uid1 uid2 = do
    mfid <- getUniqueFriendshipId uid1 uid2
    maybe' mfid (throwM NeverFriends) f
      where
        f fid = (map (friendshipLogCreatedAt . entityVal)) <$> selectList
            [ FriendshipLogFriendship ==. fid
            , FriendshipLogAction ==. AcceptRequest ]
            [ Desc FriendshipLogCreatedAt ]

-- | Returns both the UserId's of a user's friends, but also the Friendship Id
-- of that particular relation. Returning both may be redundant in some
-- situations, but it costs very little to use both since they are involved in
-- the same query. Results are returned in no particular order.

-- This function needs to use better datatypes, because this can be a fairly
-- large query. Conduit should be looked into, as well as Data.Vector.
listFriendIds :: UserId -> DB [(UserId, FriendshipId)]
listFriendIds uid = do
    res1 <- selectList [ FriendshipFirstUser  ==. uid ] []
    res2 <- selectList [ FriendshipSecondUser ==. uid ] []
    let f g (Entity fid val) = (g val, fid)
    return $ (f friendshipSecondUser <$> res1)
             ++ (f friendshipFirstUser <$> res2)


type FirstName = Text
type LastName = Text

listFriendNames :: UserId -> DB [(FirstName, LastName, UserId)]
listFriendNames = error "listFriendNames is not implemented"
