module Handler.Home where

import Import

getHomeR :: Handler Value
getHomeR = do
    sess <- getSession
    returnJson . show $ sess
