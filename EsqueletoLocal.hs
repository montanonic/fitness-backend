module EsqueletoLocal
    ( module EsqueletoLocal
    ) where
-- | This module is used to avoid name-clashes between Esqueleto and Persistent,
-- along with any other name-clashes with the Yesod scaffold.

import Import hiding (Value, Update)

import Database.Esqueleto.Internal.Language (Update)

import Database.Esqueleto as EsqueletoLocal hiding ((==.), (=.), update, on)

import qualified Database.Esqueleto as E

--
-- Avoid clashes with Import
--

on_ :: Esqueleto query expr backend => expr (Value Bool) -> query ()
on_ = E.on

--
--  Avoid clashes with Database.Persist
--

infix 4 .==.
(.==.) :: forall (query :: * -> *) (expr :: * -> *) backend typ.
    (PersistField typ, Esqueleto query expr backend) =>
    expr (Value typ) -> expr (Value typ) -> expr (Value Bool)
(.==.) = (E.==.)

infixr 3 .=.
(.=.) :: (PersistEntity val, PersistField typ, Esqueleto query expr backend) =>
    EntityField val typ -> expr (Value typ) -> expr (Update val)
(.=.) = (E.=.)

update_ :: (PersistEntity val, MonadIO m,
    PersistEntityBackend val ~ SqlBackend) =>
    (SqlExpr (Entity val) -> SqlQuery ()) -> SqlPersistT m ()
update_ = E.update
