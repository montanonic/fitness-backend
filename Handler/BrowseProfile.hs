module Handler.BrowseProfile where

import Import

-- | This handler must soon contain (1) Logic that prevents users from seeing
-- most information from profiles that aren't in their friend group, (2) logic
-- that allows users to hide any piece of Profile information from their
-- friends. The privacy system can become even more fine-grained in the future
-- (perhaps as an `operational` monad), but for now, those two levels seem
-- necessary.
getBrowseProfileR :: UserId -> Handler Value
getBrowseProfileR userId =
    runDB $ getBy404 (UniqueProfile userId) >>= returnJson
