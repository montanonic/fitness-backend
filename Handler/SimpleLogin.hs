module SimpleLogin where

import Import

-- this doesn't really work at all
postSimpleLoginR :: Text -> Handler ()
postSimpleLoginR ident = do
    redirect (AuthR (PageR dummyAuth [ident]))
