module Handler.Conversations where

import Import

import Model.Conversation

-- | Just gets Ids for now. In the future, whatever data the mobile client needs
-- will be returned by this function.
getConversationsR :: Handler Value
getConversationsR = do
    you <- requireAuthId
    cids <- runDB $ getConversationIds you
    returnJson cids

-- | Create a new conversation between yourself and the Users listed in the
-- JSON request body.
postConversationsR :: Handler ()
postConversationsR = do
    (msg, them) <- requireJsonBody :: Handler (Text, [UserId])
    you <- requireAuthId
    now <- liftIO getCurrentTime
    void $ runDB $ createNewConversation now you them msg
