module Handler.ConversationContents where

import Import

import Model.Conversation

-- | returns all of the messages tied to a particular conversation entity.
--
-- #TODO: add paginitation to prevent unnecessarily large response bodies. Users
-- will rarely need to go too far back in a conversation's history, so we should
-- only send out the messages in chunks.
getConversationContentsR :: ConversationId -> Handler Value
getConversationContentsR cid = do
    you <- requireAuthId
    msgs <- runDB $ getConversationMessages you cid
    returnJson msgs

-- | post a new message to the conversation
postConversationContentsR :: ConversationId -> Handler ()
postConversationContentsR cid = do
    msg <- requireJsonBody :: Handler Text
    you <- requireAuthId
    now <- liftIO getCurrentTime
    runDB $ addMessage now cid you msg
