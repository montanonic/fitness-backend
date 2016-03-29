module Handler.ConversationAddUser where

import Import

import Model.Conversation

postConversationAddUserR :: ConversationId -> UserId -> Handler ()
postConversationAddUserR cid them = do
    you <- requireAuthId
    now <- liftIO getCurrentTime
    runDB $ addUser now cid you them
