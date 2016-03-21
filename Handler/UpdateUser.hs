module Handler.UpdateUser where

import Import

-- | We're going to want to make sure any actual changes get logged, so the code
-- for this absolutely must be bulked up. Furthermore, because logging can be
-- abused to dump trash data by repeatedly sending requests which alter code
-- (from a malicious client), we must ensure that we have some protocol to guard
-- against that, timeouts being the most obvious way, but also tracking repeat
-- offenders. This may warrant its own table in the future, as more types of
-- threats are uncovered, and we seek to keep track of potentially dangerous
-- or malicious "users".
postUpdateUserR :: Handler ()
postUpdateUserR = do
    userId <- requireAuthId
    user <- requireJsonBody :: Handler User
    runDB $ replace userId user
    sendResponse ("UPDATED" :: Text)
