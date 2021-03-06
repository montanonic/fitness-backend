module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Yesod.Auth.Dummy (authDummy)
import Yesod.Auth.Message (AuthMessage(IdentifierNotFound) )
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding
-- -and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    -- Yesod Middleware allows you to run code before and after each handler
    -- function. The defaultYesodMiddleware adds the response header "Vary:
    -- Accept, Accept-Language" and performs authorization checks. Some users
    -- may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either
    --      a header or POST parameter.
    --
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module
    -- of the yesod-core package.
    yesodMiddleware = defaultYesodMiddleware

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            --addStylesheet $ StaticR css_bootstrap_css
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")


    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

--
-- ## Authorization code
--

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR (PluginR "dummy" [])

    -- Routes not requiring authentication.
    -- True filters for requests that are Write-requests, and False is for
    -- Read-only requests.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized

    isAuthorized HomeR _ = return Authorized
    -- no login is required to create a User currently, though in production
    -- we must definitely tie in authentication to the same stage as user
    -- creation, otherwise malicious clients could spam our database by creating
    -- new users.
    isAuthorized UserR True = return Authorized -- #TODO: this should be changed
        -- to require some form of authentication first, or something with a
        -- similar effect. The issue is that we don't want people spam-creating
        -- accounts, but perhaps modifying that within the authorization code
        -- won't actually be the best way to handle this.

    isAuthorized (BrowseProfileR _) _ = return Authorized -- #TODO: update this
        -- route block access according to the queried user's privacy settings.

    isAuthorized (FriendshipsR _) _ = return Authorized -- #TODO: so this might
        -- actually need a bit more nuance to it. We don't want a user to see
        -- another user's friendships if that user has decided to keep that
        -- information private. Hence, this function should fail when accessing
        -- UserId's whose privacy settings prevent the queryer from seeing those
        -- relationships.
    isAuthorized (DefriendR _)            _ = isLoggedIn
    isAuthorized (CreateFriendRequestR _) _ = isLoggedIn
    isAuthorized (CancelFriendRequestR _) _ = isLoggedIn
    isAuthorized (AcceptFriendRequestR _) _ = isLoggedIn


    -- #NOTE: consider checking to make sure that a user is an active
    -- participant of a conversation in order to view it. Instead of relying on
    -- Model code to throw an exception in that case, it may make more sense to
    -- move a check like that to the Authorization layer.
    --
    -- On the flipside, if we consider accessing a conversation to be an
    -- inherently private affair, only granted to those who are members of it,
    -- it may make just as much sense to force that every call of such a
    -- function requires that they be a member of it. This is honeslty a very
    -- mixed case right now, and it's not important enough to need to decide
    -- either way on just yet. For now, the Model code will retain the checks.

    -- The catch-all auth code is to make sure that the calling user is logged
    -- in, which is currently all that is really required to use any of these
    -- handlers.
    --isAuthorized (ConversationsR cid) _
    --isAuthorized (ConversationContentsR cid) _
    --isAuthorized (ConversationAddUserR cid) _


    -- All other routes require being logged in.
    isAuthorized _ _ = isLoggedIn

isLoggedIn = do
    maid <- maybeAuthId
    case maid of
        Nothing -> return AuthenticationRequired
        Just _ -> return Authorized

-- | We want to ensure that on certain routes, such as UserR, clients can only
-- query and modify resources that are their own. This is necessary in order to
-- ensure some level of privacy by preventing clients from accidentally
-- accessing resources that would impinge upon that privacy.
isSameUser userId = do
    aid <- requireAuthId
    if aid == userId
        then return Authorized
        else return $ Unauthorized "You cannot query or modify User information\
            \ other than your own."

--------------------------------------------------------------------------------

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer _ = True

    authenticate creds = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (Entity uid _) -> return $ Authenticated uid
            Nothing ->
                return $ UserError $ IdentifierNotFound (credsIdent creds)

    -- You can add other plugins like Google Email, email or OAuth here
    authPlugins _ = [authDummy]

    authHttpManager = getHttpManager

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
--
-- This can also be useful for writing code that works across multiple Yesod
-- applications.
instance HasHttpManager App where
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
