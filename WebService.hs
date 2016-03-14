module WebService where

{- https://hbtvl.wordpress.com/2015/06/28/servant-persistent-and-dsls/ -}
import Import

import Database.Persistent
import Control.Monad.Operational (Program, view, singleton
    , ProgramViewT(Return, (:>>=)))

{-
--------------------------------------------------------------------------------
--------------------------------------DATA--------------------------------------
--------------------------------------------------------------------------------


-- | Persistent Constraints; these constraints are required for interop with
-- the persistent backend.
type PC val = (PersistEntityBackend val ~ SqlBackend, PersistEntity val)

type WebService = Program WebAction

data WebAction a where
    --Throw :: ServantErr               -> WebAction a
    New   :: PC val => val            -> WebAction (Key val)
    Get   :: PC val => Key val        -> WebAction (Maybe val)
    GetBy :: PC val => Unique val     -> WebAction (Maybe (Entity val))
    Upd   :: PC val => Key val -> val -> WebAction ()
    Del   :: PC val => Key val        -> WebAction ()
{-
-- throws an error
wthrow :: ServantErr -> WebService a
wthrow = singleton . Throw
-}

-- dual of `persistent`'s `insert`
wnew :: PC val => val -> WebService (Key val)
wnew = singleton . New

-- dual of `persistent`'s `get`
wget :: PC val => Key val -> WebService (Maybe val)
wget = singleton . Get

-- dual of `persistent`'s `getBy`
wgetBy :: PC val => Unique val ->  WebService (Maybe (Entity val))
wgetBy = singleton . GetBy

-- dual of `persistent`'s `update`
wupd :: PC val => Key val -> val -> WebService ()
wupd k v = singleton (Upd k v)

-- dual of `persistent`'s `delete`
wdel :: PC val => Key val -> WebService ()
wdel = singleton . Del

-- like `wget` but throws a 404 if it could not find the corresponding record
wgetOr404 :: PC val => Key val -> WebService val
wgetOr404 = wget >=> maybe (wthrow err404) return

-- like `wgetBy` but throws a 404 if it could not find the corresponding record
wgetByOr404 :: PC val => Unique val -> WebService (Entity val)
wgetByOr404 = wgetBy >=> maybe (wthrow err404) return


--------------------------------------------------------------------------------
-----------------------------------INTERPRETERS---------------------------------
--------------------------------------------------------------------------------

-- | This is what our effectful interpreters use.
type ServantIO a = SqlPersistT (LoggingT (EitherT ServantErr IO)) a

-- | A web action through the persistent backend and to the Server.
runYesod :: WebService a -> ServantIO a
runYesod ws = case view ws of
    Return a -> return a
    Throw err@(ServantErr httpCode reasonPhrase _ _) :>>= _ -> do
        conn <- ask
        liftIO $ connRollback conn (getStmtConn conn)
        logOtherNS "WS" LevelError (show (httpCode, reasonPhrase))
        throwError err
    New v    :>>= k -> insert v     >>= runServant . k
    Get pk   :>>= k -> get pk       >>= runServant . k
    GetBy u  :>>= k -> getBy u      >>= runServant . k
    Upd pk v :>>= k -> replace pk v >>= runServant . k
    Del v    :>>= k -> delete v     >>= runServant . k


-------------------------------------CRUD---------------------------------------

type CRUD a =
         ReqBody '[JSON] a     :> Post '[JSON] (SKey a) -- create
    :<|> Capture "id" (SKey a) :> Get '[JSON] a -- read
    :<|> Capture "id" (SKey a) :> ReqBody '[JSON] a :> Put '[JSON] () -- update
    :<|> Capture "id" (SKey a) :> Delete '[JSON] () -- delete

{- runCrudWith :: ConnectionPool -> (Connection -> a -> IO ()) -> (Connection ->
SKey a -> IO (Maybe a)) -> (Connection -> SKey a -> a -> IO ()) -> (Connection ->
SKey a -> IO ()) -> Server (CRUD a)

automatic CRUD: http://lpaste.net/153940  by <alpounet>
not working, but right direction -}

runCrud :: (PersistEntity val, ToBackendKey SqlBackend val)
    => ConnectionPool -- ^ Connection pool
    -> Server (CRUD val)

    --Server _
runCrud pool =
    runNew :<|> runGet :<|> runUpd :<|> runDel
  where
    runNew val = runQuery $ do
        sk <- toSKey <$> wnew val
        return sk

    runGet sk = runQuery $ do
        wgetOr404 (toPKey sk)

    runUpd sk val = runQuery $ do
        wupd (toPKey sk) val

    runDel sk = runQuery $ do
        wdel (toPKey sk)

    -- | Look at the type of ServantIO. This function takes a WebService and
    -- runs our effectful runServant function on the service, and we then
    -- peel away the LoggingT and SqlPersistT layers until only the servant
    -- action remains.
    runQuery :: WebService a -> EitherT ServantErr IO a
    runQuery ws = runDB $ runSqlPool (runServant ws) pool
-}
