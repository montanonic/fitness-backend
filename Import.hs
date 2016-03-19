module Import
    ( module Import
    ) where

import Foundation            as Import
import Import.NoFoundation   as Import

-- | Actions which only require access to the database connection can be given
--   type @DB a@ (as opposed to @YesodDB App a@). This allows them to also be
--   called in tests.
type DB a = forall (m :: * -> *). (MonadIO m, Functor m) => ReaderT SqlBackend m a
