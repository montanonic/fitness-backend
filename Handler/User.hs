module Handler.User where

import Import

-- User

getUserR :: UserId -> Handler Value
getUserR userId = runDB $ get404 userId >>= returnJson

putUserR :: UserId -> Handler Value
putUserR userId = do
    user <- requireJsonBody :: Handler User
    runDB $ replace userId user
    sendResponse ("UPDATED" :: Text)

deleteUserR :: UserId -> Handler Value
deleteUserR userId = do
    runDB $ delete userId
    sendResponse ("DELETED" :: Text)

-- NewUser

postNewUserR :: Handler Value
postNewUserR = do
    user <- requireJsonBody :: Handler User
    key <- runDB $ insert user
    sendResponseCreated (UserR key)
