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

import Data.Time.Clock (UTCTime( utctDay ))
import Data.Time.Calendar (diffDays)

import Model.Persistent (FriendshipAction(..))
import Model.Notifications (createFriendRequestNotification)

data FriendshipException
    = AlreadyFriends
    | NotCurrentlyFriends
    | NeverFriends
    | CannotBeFriendsWithSelf
    | YouWereNotSenderOfRequest
    | RequestAlreadyExists
    | LastRequestNotExpired
instance Show FriendshipException where
    show AlreadyFriends = "You are already friends."
    show NotCurrentlyFriends = "You are not currently friends with this person."
    show NeverFriends = "These users have never been friends."
    show CannotBeFriendsWithSelf = "You can't be friends with yourself. (At\
            \ least not in this app.)"
    show YouWereNotSenderOfRequest = "You can't cancel a request you didn't\
        \ send."
    show RequestAlreadyExists = "There is already a pending friend request\
        \ between these users."
    show LastRequestNotExpired = "The last friendship request sent by the user\
        \ has not expired yet."
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

{- Obsolete for now, given extra database overhead. Better implemented manually
and generalized only if clear pattern emerges.
-- | Helper combinator that throws an exception if the two users given are
-- already friends in the database.
cannotAlreadyBeFriends :: Eq a => (a -> a -> DB b) -> a -> a -> DB b
cannotAlreadyBeFriends f uid1 uid2 = do
    fid <- getUniqueFriendshipId uid1 uid2
    mfriendshipLog <- get fid
    case mfriendshipLog of
        Nothing -> f uid1 uid2
        Just log ->
-}

-- | This function, along with its Id-only version, queries the database for
-- the friendship entity corresponding to two users.
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

-- | This constant determines how long (in Days) after a friend request is sent
-- and pending before it is automatically cancelled and a new request can be
-- sent. Do keep in mind that the only other way to cancel a friend request is
-- if the original sender chooses to cancel it. Otherwise, you may only ignore
-- it.
expirationTime :: Integer
expirationTime = 30 -- about one month

-- | #TODO: add a proper description
createFriendRequest :: UTCTime -> YourId -> TheirId -> DB ()
createFriendRequest now you them
    | you == them = throwM CannotBeFriendsWithSelf
    | otherwise = do
        -- create a new Friendship with isRequest flagged as active to signify
        -- that it is a Friend Request. If a Friendship entity already exists
        -- between these users, return that existing entity as Left instead of
        -- creating a new one:
        efid <- insertBy $ Friendship you them False True now
        case efid of
            -- new friendship was created
            Right fid -> logAndNotify now you them fid

            -- friendship already exists
            Left (Entity fid Friendship{..}) -- current friendship.
                -- first, we make sure that we don't create a new request if one
                -- already exists, or if these users are already friends:
                | friendshipIsActive == True -> throwM AlreadyFriends
                | friendshipIsRequest == True -> throwM RequestAlreadyExists
                | otherwise -> do
                    -- and if those exceptions don't arise, we check the logs to
                    -- see when the last friend request sent by the current
                    -- sender was, if ever:
                    mlog <- selectFirst [ FriendshipLogFriendship ==. fid
                                        , FriendshipLogUser ==. you
                                        , FriendshipLogAction ==. SendRequest ]
                                        [ Desc FriendshipLogCreatedAt ]
                    case mlog of
                        -- if there was no previous friend request sent by the
                        -- current user, then we make one with no further
                        -- checks:
                        Nothing -> do
                            update fid [ FriendshipIsRequest =. True ]
                            logAndNotify now you them fid

                        -- otherwise, we defer the logic to the
                        -- `okayToCreateNewFriendRequest` function defined
                        -- below, which prevents requests from getting sent too
                        -- often or too rapidly, and warns clients:
                        Just lastRequest ->
                            okayToCreateNewFriendRequest lastRequest

    where
        -- following the modification or creation of the Friendship entity to
        -- signify a Friend Request is active, we make a log to record that a
        -- request was made, and then create a notification for the receiver
        -- of the request.
        logAndNotify :: UTCTime -> YourId -> TheirId -> FriendshipId -> DB ()
        logAndNotify now' you' them' fid' = do
            insert_ $ FriendshipLog fid' you' SendRequest now'
            createFriendRequestNotification now' you' them'

        -- this is where we decide if it's okay to send another Friend Request,
        -- knowing that this user has sent at least one previous request, the
        -- latest of which is this function's first parameter.
        --
        -- Things this function prevents: #TODO
        okayToCreateNewFriendRequest :: Entity FriendshipLog -> DB ()
        okayToCreateNewFriendRequest lastRequest = undefined


-- | Needs logic to remove notification from the person you sent the request to.
cancelFriendRequest :: UTCTime -> YourId -> Entity Friendship -> DB FriendshipId
cancelFriendRequest now you (Entity fid fship)
    | friendshipFirstUser fship /= you = throwM YouWereNotSenderOfRequest
    | friendshipIsActive fship == True = throwM AlreadyFriends
    | otherwise = do
        update fid [ FriendshipIsRequest =. False ]
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
