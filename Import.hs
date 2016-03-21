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

-- | Transform a raw string of Text (that is, formatted according to how it
-- appears in code) as a single string of Text. This allows for using the
-- multiline string quasiquoters, such as [s| |], to avoid having to add line
-- breaks with an escape character manually, while also keeping the Text in the
-- simplistic single line format, with no carriage returns.
asSingleLine :: Text -> Text
asSingleLine = mconcat . ((++ " ") <$>) . lines

-- | A version of maybe that takes the Maybe value first. I find my code would
-- often feel cleaner if I could use that value first.
maybe' :: Maybe a -> b -> (a -> b) -> b
maybe' m def f = maybe def f m
