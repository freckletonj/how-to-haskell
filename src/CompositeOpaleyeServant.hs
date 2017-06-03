{- stack
--resolver lts-8.2
--install-ghc
exec ghci
--package aeson
--package composite-base
--package composite-aeson
--package text
--package string-conversions
--package postgresql-simple
--package vinyl
--package servant
--package servant-server
--package warp
--package bytestring
--package natural-transformation
-}

{-# LANGUAGE
Arrows
, DataKinds
, OverloadedStrings
, PatternSynonyms
, TypeOperators
, TemplateHaskell
, FlexibleContexts
, RankNTypes


-- most of the following came into play when I wrote `RecWith`
, ConstraintKinds
, TypeSynonymInstances
, FlexibleInstances
, MultiParamTypeClasses
, TypeFamilies
, GeneralizedNewtypeDeriving
#-}

import Data.Vinyl (RElem)
import Data.Functor.Identity (Identity)
import Data.Vinyl.TypeLevel (RIndex)
import Composite.Aeson (JsonFormat, defaultJsonFormatRec
                       , recJsonFormat, toJsonWithFormat
                       , DefaultJsonFormat(defaultJsonFormat)
                       , wrappedJsonFormat, textJsonFormat)
import Composite.Aeson.TH (makeRecJsonWrapper)
import Composite.Opaleye (defaultRecTable)
import Composite.Record (Record, Rec(RNil), (:->), pattern (:*:))
import Composite.TH (withOpticsAndProxies)
import Control.Arrow (returnA)
import Control.Lens (view, iso, makeWrapped)
import Control.Lens.Wrapped 
--import Control.Lens.TH (makeWrapped)
import Data.Int (Int64)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import Opaleye
import Opaleye.Internal.TableMaker (ColumnMaker)
import Data.String.Conversions (cs)
import qualified Data.Aeson as Aeson
import Data.Profunctor.Product.Default (Default(def))
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.ByteString (ByteString)
import Control.Exception (bracket)
import Control.Monad.Base (liftBase)

-- | Db
import Data.Pool (Pool, withResource)
import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.Simple as PGS -- used for printSql

-- | Web
import Servant 
--import Servant.Server
import Control.Natural ((:~>)(NT))
import Control.Natural 
import Network.Wai.Handler.Warp (run)





--------------------------------------------------
-- | Types
-- 1. declare `F`ields, and `C`olumns
-- 2. generate proxies, lenses, and prisms
--
-- Eg:
--
-- @
--   type FFoo = "foo" :-> Int
--   fFoo :: FFoo ∈ rs => Lens' (Record rs) Int
--   fFoo = rlens fFoo_
--   _FFoo :: FFoo ∈ rs => Prism' (Field rs) Int
--   _FFoo = fieldPrism fFoo_ . _Wrapped
--   fFoo_ :: Proxy FFoo
--   fFoo_ = Proxy
-- @


-- | Newtype ClearPassword so it can't be passed around as ordinary Text
newtype ClearPassword a = ClearPassword {unClearPassword :: a}

instance DefaultJsonFormat a => DefaultJsonFormat (ClearPassword a) where
  defaultJsonFormat = wrappedJsonFormat defaultJsonFormat
instance Wrapped (ClearPassword a) where
  type Unwrapped (ClearPassword a) = a
  _Wrapped' = iso unClearPassword ClearPassword
  
withOpticsAndProxies [d|
  type FEmail = "email" :-> Text
  type CEmail = "email" :-> Column PGText

  type FAge = "age" :-> Text
  type CAge = "age" :-> Column PGText

  type FClearPassword = "clearpass" :-> ClearPassword Text
  type CHashPassword = "hashpass" :-> Column PGText
  |]

--------------------------------------------------
-- The Stack
--   Note: I had to move this here because TH stuff was goofing around

data AppData = AppData { appConnPool :: Pool PGS.Connection }

type AppStackM = ReaderT AppData Handler

--------------------------------------------------
-- | Db Setup

-- | Helper Fn
printSql :: Default Unpackspec a a => Query a -> IO ()
printSql = putStrLn . maybe "Empty query" id . showSqlForPostgres

-- | Db Records
type DbUser = '[CEmail, CAge]
type DbPassword = '[CEmail, CHashPassword]

userTable :: Table (Record DbUser) (Record DbUser)
userTable = Table "user" defaultRecTable

passwordTable :: Table (Record DbPassword) (Record DbPassword)
passwordTable = Table "password" defaultRecTable

withDb :: (MonadBaseControl IO m, MonadReader AppData m)
       => (PGS.Connection -> IO a)
       -> m a
withDb action = do
  pool <- asks appConnPool
  liftBase $ withResource pool action


--------------------------------------------------
-- | Other Records

type User = '[FEmail, FAge]

-- Create: newtype ApiUser = ApiUser {unApiUser :: Record User}
makeRecJsonWrapper "ApiUser" ''User


makeWrapped ''ApiUser

-- | Client-submitted credentials for Authentication
type AuthUser = '[FEmail, FClearPassword]

-- Create: newtype ApiAuthUser = ApiAuthUser {unApiAuthUser :: Record AuthUser}
makeRecJsonWrapper "ApiAuthUser" ''AuthUser


makeWrapped ''ApiAuthUser
--


--------------------------------------------------
-- Query

-- | A constraint for a record `rs` that has a field `f`
-- TODO: generalize the getter & setter portions?
type RecWith f rs = (Default ColumnMaker (Record rs) (Record rs),
                     RElem f rs (RIndex f rs))

-- | Can be used to query any table with a CEmail
queryByEmail :: (RecWith CEmail rs) =>
                Table a (Record rs) -> Text -> QueryArr () (Record rs)
queryByEmail table email = proc () -> do
  u <- queryTable table -< ()
  let uEmail = view cEmail u
  restrict -< uEmail .=== constant email
  returnA -< u
  

--------------------------------------------------
-- Tests

-- SELECT <user cols> FROM "user" ...
queryUserTest = printSql $ queryByEmail userTable "hi"

-- SELECT <password cols> FROM "password" ...
queryPasswordTest = printSql $ queryByEmail passwordTable "hi"




--------------------------------------------------
-- | Hash
-- note: you probably want better security than this

hashPassword :: ClearPassword Text -> Text
hashPassword (ClearPassword "secret") = "garbledHash"
hashPassword _ = "hashOfNotSecret"

validatePassword :: ClearPassword Text -> Text -> Bool
validatePassword clear hash = (hashPassword clear) == hash

-- validatePassword (ClearPassword "secret") "garbledHash" == True


--------------------------------------------------
-- | Servant

-- type Handler a = ExceptT ServantErr IO a
-- type Server (api :: k) = ServerT api Handler
-- serveWithContext :: HasServer api context
--                     => Proxy api -> Context context -> Server api -> Application
-- serve :: HasServer api '[] => Proxy api -> Server api -> Application
-- newtype (:~>) (m :: * -> *) (n :: * -> *) = Nat {unNat :: forall a. m a -> n a}
-- enter :: Servant.Utils.Enter.Enter typ arg ret => arg -> typ -> ret

type LoginRoute = ReqBody '[JSON] ApiAuthUser
                  :> Post '[JSON] ApiUser

loginRoute :: ApiAuthUser -> AppStackM ApiUser
loginRoute =
  --let email = view fEmail authUser
  --user <- return $ queryByEmail passwordTable email
  --let clearPassword = view fClearPassword authUser
  --    hash = view cHashPassword user
  return undefined

type Api = "test" :> LoginRoute
           -- :<|> "test2" :> LoginRoute
  
api :: Proxy Api
api  = Proxy

service :: ServerT Api AppStackM
service = loginRoute

  
  -- (\authUser -> do
  -- users <- withDb $ \ conn ->
  --   runQuery conn $ proc () -> do
  --     user <- queryByEmail userTable "hi" -< ()
  --     restrict -< (view cEmail user) .=== (constant $ view fEmail authUser)
  --     returnA -< user
  -- return undefined
  --   )


--------------------------------------------------
-- App Machinery

withPostgresqlPool :: MonadBaseControl IO m
  => ByteString
  -> Int
  -> (Pool PGS.Connection -> m a)
  -> m a
withPostgresqlPool connStr nConns action = do
  stm <- liftBaseWith $ \ runInBase ->
    bracket createPool Pool.destroyAllResources (runInBase . action)
  restoreM stm
  where
    createPool = Pool.createPool (PGS.connectPostgreSQL connStr) PGS.close 1 20 nConns

startApp :: IO ()
startApp = do
  withPostgresqlPool "host=localhost port=5432 user=god dbname=vinyleye" 2
    $ \connPool -> do
      let appData = AppData connPool
      run 8080 $ serve api (appServer appData)
      -- liftIO . run 8080 $ serve api
      --   $ enter (appStackToHandler appData) service

appServer :: AppData -> Server Api
appServer appData = enter (appStackToHandler appData) service
      
appStackToHandler' :: forall a. AppData -> AppStackM a -> Handler a
appStackToHandler' appData action = runReaderT action appData

appStackToHandler :: AppData -> (AppStackM Servant.:~> Handler)
appStackToHandler appData = Nat $ appStackToHandler' appData
