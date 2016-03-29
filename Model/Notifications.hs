module Model.Notifications
    ( 
    ) where
-- | NOTE: Notifications are currently low-priority, given the need to integrate
-- the system with AWS. Efforts will be focused on creating server functionality
-- for all data structures, and handling Notifications at a later point in time.
import Import

import Model.Persistent (NotificationType(..))

data NotificationException
    = UnknownNotificationType
instance Show NotificationException where
    show UnknownNotificationType = "The notification type provided is unknown.\
        \ Please check models to ensure that it is a real entity, and if so,\
        \ create a new notification type in Model.Persistent."

{-
createNotification :: NotificationType -> UTCTime -> UserId -> DB NotificationId
createNotification typ now uid = do
    insert $ Notification uid typ now
    case typ of
        FriendRequest -> insert
        _ -> throwM UnknownNotificationType
-}

createFriendRequestNotification ::
    UTCTime -> UserId -> UserId -> DB ()
createFriendRequestNotification now sender receiver = do
    nid <- insert $ Notification receiver FriendRequest False now
    insert_ $ FriendRequestNotification nid sender False

friendshipAcceptedNotification = error "friendshipAcceptedNotification\
    \ is unimplemented"

ignoreFriendshipNotification = error "ignoreFriendshipNotification\
    \ is unimplemented"

friendRequestWasAccepted :: UTCTime -> UserId -> UserId -> DB ()
friendRequestWasAccepted now madeReq acceptedReq = error
    "friendRequestWasAccepted is unimplemented"
