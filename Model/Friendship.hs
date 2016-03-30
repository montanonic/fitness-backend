module Model.Friendship
    ( module Model.Friendship
    ) where

{- I've decided that explicit exports aren't really necessary for this module,
or honestly, *any* of the Model modules.

Old exports:

FriendshipException(..)
, createFriendRequest
, cancelFriendRequest
, acceptFriendRequest
, getUniqueFriendship
, getUniqueFriendshipId
, becameFriends
, lastBecameFriends
, listFriendIds

-}

import Import

import Data.Time.Calendar (diffDays)

import Model.Persistent (FriendshipAction(..))
--import Model.Notifications (createFriendRequestNotification)

data FriendshipException
    = AlreadyFriends
    | NotCurrentlyFriends
    | NeverFriends
    | CannotBeFriendsWithSelf
    | YouWereNotSenderOfRequest
    | YouWereNotReceiverOfRequest
    | RequestAlreadyExists
    | TooManyRequestsHaveBeenSent
    | NoFriendRequestExists
    | LastRequestNotExpired
instance Show FriendshipException where
    show AlreadyFriends = "You are already friends."
    show NotCurrentlyFriends = "You are not currently friends with this person."
    show NeverFriends = "These users have never been friends."
    show CannotBeFriendsWithSelf = "You can't be friends with yourself. (At\
            \ least not in this app.)"
    show YouWereNotSenderOfRequest = "You can't cancel a request you didn't\
        \ send."
    show YouWereNotReceiverOfRequest = "You can't accept a request you didn't\
        \ receive."
    show RequestAlreadyExists = "There is already a pending friend request\
        \ between these users."
    show TooManyRequestsHaveBeenSent = "The user has sent more friend requests\
        \ than allowed for a certain period of time, and must wait to send a\
        \ new request."
    show NoFriendRequestExists = "There is no active friend request between\
        \ these users."
    show LastRequestNotExpired = "The last friendship request sent by the user\
        \ has not expired yet."
instance Exception FriendshipException


--
-- ## Basics
--

-- | This function, along with its Id-only version, queries the database for
-- the friendship entity corresponding to two users.
getUniqueFriendship :: UserId -> UserId -> DB (Entity Friendship)
getUniqueFriendship uid1 uid2
    | uid1 == uid2 = throwM CannotBeFriendsWithSelf
    | otherwise = fromMaybeM (throwM NeverFriends) $ selectFirst
        [ FriendshipFirstUser <-. [uid1, uid2]
        , FriendshipSecondUser <-. [uid1, uid2] ] []

getUniqueFriendshipId :: UserId -> UserId -> DB FriendshipId
getUniqueFriendshipId uid1 uid2 =
    entityKey <$> getUniqueFriendship uid1 uid2

--
-- ## Friend requests.
--

-- | #TODO: add a proper description
createFriendRequest :: UTCTime -> UserId -> UserId -> DB ()
createFriendRequest now you them
    | you == them = throwM CannotBeFriendsWithSelf
    | otherwise = do
        -- create a new Friendship entity flagged as a Friend Request. If a
        -- Friendship entity already exists between these users, return that
        -- existing entity as Left instead of creating a new one:
        efid <- insertBy $ Friendship you them False True now
        case efid of
            -- new friendship was created
            Right fid -> logAndNotify now you them fid -- see `where` clause

            -- friendship already exists
            Left fe@(Entity _ Friendship{..})
                -- first, we make sure that we don't create a new request if one
                -- already exists, or if these users are already friends:
                | friendshipIsActive == True -> throwM AlreadyFriends
                | friendshipIsRequest == True -> throwM RequestAlreadyExists
                -- and if those exceptions aren't thrown, we then apply the
                -- following function, which contains the logic for allowing
                -- a friend request. See it below for more details.
                | otherwise -> okayToCreateNewFriendRequest now you them fe

    where
        -- once we create a Friend Request, we make a log to record that a
        -- request was made, and then create a notification for the receiver of
        -- the request.
        logAndNotify :: UTCTime -> UserId -> UserId -> FriendshipId -> DB ()
        logAndNotify now' you' them' fid' = do
            insert_ $ FriendshipLog fid' you' CreateRequest now'
            --createFriendRequestNotification now' you' them'
            -- NOTIFICATIONS CURRENTLY DISABLED

        -- this is where we decide if it's okay to send another Friend Request.
        -- At a certain threshold of sent friend requests (to one user) in a
        -- particular time window, we prevent new requests from being sent.
        -- once that threshold is no longer met, a user can send more requests.
        --
        -- the current threshold is: no more than three requests within one
        -- month.
        okayToCreateNewFriendRequest :: UTCTime -> UserId -> UserId ->
            Entity Friendship -> DB ()
        okayToCreateNewFriendRequest now' you' them'
          (Entity fid' Friendship{..}) = do
            -- you can send more requests than the requestLimit, but not within
            -- the requestLimitTimeWindow. Once the oldest of last three Friend
            -- requests is older than the time window, you can create another
            -- request.
            let requestLimit = 3
                requestLimitTimeWindow = 30 -- in days
                today = utctDay now'
            logs <- selectList [ FriendshipLogFriendship ==. fid'
                                , FriendshipLogUser ==. you'
                                , FriendshipLogAction ==. CreateRequest ]
                                [ Desc FriendshipLogCreatedAt
                                , LimitTo requestLimit ]
            -- #TODO: takeUntil time is > 30 days ago
            case logs of
                -- no previous requests made by this user (will only actually
                -- occur when the other user sent the original request, and
                -- they since have become defriended)
                [] -> do
                    update fid' [ FriendshipIsRequest =. True ]
                    logAndNotify now' you' them' fid'
                -- previous logs exist
                x ->
                  let
                    getRequestDay = utctDay . friendshipLogCreatedAt . entityVal
                    oldestOfLastThreeRequests = getRequestDay $ lastEx x
                    today = utctDay now'
                    daysSinceOldestRequest =
                        diffDays today oldestOfLastThreeRequests
                  in
                    if daysSinceOldestRequest < requestLimitTimeWindow
                        -- requests/time is below the limit
                        then do
                            -- create friend request, log and notify
                            update fid' [ FriendshipIsRequest =. True ]
                            logAndNotify now' you' them' fid'
                        -- otherwise, requests/time is over the limit, which
                        -- means that this user cannot send a friend request at
                        -- this time
                        else throwM TooManyRequestsHaveBeenSent


-- | Needs logic to remove notification from the person you sent the request to.
cancelFriendRequest :: UTCTime -> UserId -> UserId -> DB ()
cancelFriendRequest now you them = do
    friendshipEnt <- getUniqueFriendship you them
    cancelFriendRequest' now you friendshipEnt

cancelFriendRequest' :: UTCTime -> UserId -> Entity Friendship -> DB ()
cancelFriendRequest' now you (Entity fid Friendship{..})
    | friendshipFirstUser /= you = throwM YouWereNotSenderOfRequest
    | friendshipIsActive == True = throwM AlreadyFriends
    | friendshipIsRequest == False = throwM NoFriendRequestExists
    | otherwise = do
        update fid [ FriendshipIsRequest =. False ]
        insert_ $ FriendshipLog fid you CancelRequest now

-- | Convert the given Friendship entity currently functioning as a Friend
-- Request into an active Friendship with no request.
-- #TODO: Rewrite this function to accept a FriendshipEntity rather than
-- a FriendshipId, like cancelFriendRequest, since it's more efficient, and
-- clear that way.
acceptFriendRequest :: UTCTime -> UserId -> UserId -> DB ()
acceptFriendRequest now you them = do
    friendshipEnt <- getUniqueFriendship you them
    acceptFriendRequest' now you friendshipEnt

acceptFriendRequest' :: UTCTime -> UserId -> Entity Friendship -> DB ()
acceptFriendRequest' now you (Entity fid Friendship{..})
    | friendshipFirstUser == you = throwM YouWereNotReceiverOfRequest
    | friendshipIsActive == True = throwM AlreadyFriends
    | friendshipIsRequest == False = throwM NoFriendRequestExists
    | otherwise = do
        update fid [ FriendshipIsActive =. True
                   , FriendshipIsRequest =. False ]
        insert_ $ FriendshipLog fid you AcceptRequest now

{-
acceptFriendRequest :: UTCTime -> UserId -> FriendshipId -> DB ()
acceptFriendRequest now you fid = do
    Friendship{..} <- liftM2 fromMaybe (throwM NoFriendRequestExists) (get fid)
    when (friendshipIsActive == True) $ throwM AlreadyFriends
    if friendshipIsRequest
        then do
            when (you /= friendshipSecondUser) $
                throwM YouWereNotReceiverOfRequest
            update fid [ FriendshipIsActive =. True
                       , FriendshipIsRequest =. False ]
            insert_ $ FriendshipLog fid you AcceptRequest now
        else throwM NoFriendRequestExists
-}


ignoreFriendRequest = error "This may end up being a client-side only operation\
    \, so `ignoreFriendRequest` is being left undefined for now."

--
-- ## Friendship
--

defriend :: UTCTime -> UserId -> UserId -> DB ()
defriend now you them = do
    friendshipEnt <- getUniqueFriendship you them
    defriend' now you friendshipEnt

defriend' :: UTCTime -> UserId -> Entity Friendship -> DB ()
defriend' now you (Entity fid Friendship{..})
    | friendshipIsActive == False = throwM NotCurrentlyFriends
    | otherwise = do
        update fid [ FriendshipIsActive =. False ]
        insert_ $ FriendshipLog fid you Defriend now


-- | Shows each time a pair of Users became friends. Null represents them
-- never having been friends. Sorted from earliest to latest.
becameFriends :: UserId -> UserId -> DB [UTCTime]
becameFriends uid1 uid2 = do
    fid <- getUniqueFriendshipId uid1 uid2
    map (friendshipLogCreatedAt . entityVal <$>) $ selectList
        [ FriendshipLogFriendship ==. fid
        , FriendshipLogAction ==. AcceptRequest ]
        [ Asc FriendshipLogCreatedAt ]

-- | Same as becameFriends, but sorted from latest to earliest. These functions
-- are identical when used on friends who've never defriended one-another, or
-- users who weren't friends.
lastBecameFriends :: UserId -> UserId -> DB [UTCTime]
lastBecameFriends uid1 uid2 = do
    fid <- getUniqueFriendshipId uid1 uid2
    map (friendshipLogCreatedAt . entityVal <$>) $ selectList
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
