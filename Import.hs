module Import
    ( module Import
    ) where

import Foundation            as Import
import Import.NoFoundation   as Import

-- | Actions which only require access to the database connection can be given
--   type @DB a@ (as opposed to @YesodDB App a@). This allows them to also be
--   called in tests.
type DB a = forall (m :: * -> *).
    (MonadIO m, MonadThrow m, Functor m) => SqlPersistT m a

-- Persistent Constraints
type PC val = (PersistEntity val, PersistEntityBackend val ~ SqlBackend)

-- | Transform a raw string of Text (that is, formatted according to how it
-- appears in code) as a single string of Text. This allows for using the
-- multiline string quasiquoters, such as [s| |], to avoid having to add line
-- breaks with an escape character manually, while also keeping the Text in the
-- simplistic single line format, with no carriage returns.
asSingleLine :: Text -> Text
asSingleLine = mconcat . ((++ " ") <$>) . lines

--
-- * Monads and Maybe utilities, courtesy of
-- http://haddock.stackage.org/lts-5.9/Agda-2.4.2.5/src/Agda-Utils-Maybe.html#fromMaybeM
--

-- | Monadic version of 'maybe'.

maybeM :: Monad m => m b -> (a -> m b) -> m (Maybe a) -> m b
maybeM n j mm = maybe n j =<< mm

-- | Monadic version of 'fromMaybe'.

fromMaybeM :: Monad m => m a -> m (Maybe a) -> m a
fromMaybeM m mm = maybeM m return mm

-- | Monadic version of 'caseMaybe'.
--   That is, 'maybeM' with a different argument ordering.
caseMaybeM :: Monad m => m (Maybe a) -> m b -> (a -> m b) -> m b
caseMaybeM mm d f = maybeM d f mm

-- | 'caseMaybeM' with flipped branches.
ifJustM :: Monad m => m (Maybe a) -> (a -> m b) -> m b -> m b
ifJustM mm = flip (caseMaybeM mm)

-- | A more telling name for 'Traversable.forM_' for the 'Maybe' collection type.
--   Or: 'caseMaybe' without the 'Nothing' case.
whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust m k = caseMaybe m (return ()) k

-- | 'caseMaybe' without the 'Just' case.
whenNothing :: Monad m => Maybe a -> m () -> m ()
whenNothing m d = caseMaybe m d (\_ -> return ())

-- | 'caseMaybeM' without the 'Nothing' case.
whenJustM :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
whenJustM c m = c >>= (`whenJust` m)

-- | 'caseMaybeM' without the 'Just' case.
whenNothingM :: Monad m => m (Maybe a) -> m () -> m ()
whenNothingM mm d = mm >>= (`whenNothing` d)
