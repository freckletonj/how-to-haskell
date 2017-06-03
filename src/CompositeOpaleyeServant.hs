{- stack
--resolver lts-8.2
--install-ghc
exec ghci
--package aeson
--package composite-base
--package composite-aeson
--package text
--package string-conversions
--package postgres-simple
--package vinyl
--package servant
--package servant-server
--package warp
--package bytestring

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
import Control.Lens (view, iso)
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
makeRecJsonWrapper "ApiUser" ''User
--makeWrapped ''ApiUser

-- | Client-submitted credentials for Authentication
type AuthUser = '[FEmail, FClearPassword]
makeRecJsonWrapper "ApiAuthUser" ''AuthUser
--makeWrapped ''ApiAuthUser

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

type LoginRoute = ReqBody '[JSON] ApiAuthUser
                  :> Post '[JSON] ApiUser

loginRoute :: Record AuthUser -> Handler (Record User)
loginRoute authUser = do
  let email = view fEmail authUser
  --user <- return $ queryByEmail passwordTable email
  --let clearPassword = view fClearPassword authUser
  --    hash = view cHashPassword user
  return undefinedXXX

type Api = LoginRoute

api :: Proxy Api
api = Proxy

service :: ServerT Api AppStackM
service = do
  undefinedXXX
  
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
      liftIO . run 8080 $ serve api
        $ enter (appStackToHandler appData) service
    
appStackToHandler' :: forall a. AppData -> AppStackM a -> Handler a
appStackToHandler' appData action = runReaderT action appData

appStackToHandler :: AppData -> (AppStackM :~> Handler)
appStackToHandler appData = Nat $ appStackToHandler' appData

-- ServerT (SecurityApi '[JWT, Cookie]) (ExceptT ServantErr IO)

-- Network.Wai.Handler.Warp.run 8080
--     $ serveWithContext mainApi serverCfg
--     $ mainServer
--     pool
--     defaultCookieSettings
--     jwtCfg
--     githuboa
--     bitbucketoa

