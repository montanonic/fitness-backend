module Handler.ConversationAddUser where

import Import

import Model.Conversation

postConversationAddUserR :: ConversationId -> UserId -> Handler ()
postConversationAddUserR cid them = do
    you <- requireAuthId
    now <- liftIO getCurrentTime
    Profile {..} <- runDB $ do
        addUser now cid you them
        -- get their profile
        entityVal <$> (getBy404 $ UniqueProfile them)
    sendResponse (profileFirstName ++ " " ++ profileLastName ++ " was\
        \ successfully added to the conversation" :: Text)
