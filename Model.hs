{-# LANGUAGE FlexibleInstances #-}

module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi

import Model.Persistent

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/

-- Rationale for the changes made, here:
-- http://haddock.stackage.org/lts-5.8/serversession-backend-persistent-1.0.2/Web-ServerSession-Backend-Persistent.html
share [mkPersist sqlSettings, mkSave "entityDefs"]
    $(persistFileWith lowerCaseSettings "config/models")
