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
import Network.Wai.Handler.Warp (run)

import Debug.Trace (trace)


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

-- Create instances for newtype
makeWrapped ''ClearPassword
instance DefaultJsonFormat a => DefaultJsonFormat (ClearPassword a) where
  defaultJsonFormat = wrappedJsonFormat defaultJsonFormat

withOpticsAndProxies [d|
  type FEmail = "email" :-> Text
  type CEmail = "email" :-> Column PGText

  type FAge = "age" :-> (Maybe Int)
  type CAge = "age" :-> Column (Nullable PGInt4)

  type FClearPassword = "clearpass" :-> ClearPassword Text
  type FHashPassword = "hashpass" :-> Text
  type CHashPassword = "hashpass" :-> Column PGText
  |]


--------------------------------------------------
-- The Apps Monad Stack
--   Note: I had to move this here because TH stuff was goofing around

-- | Lives in Reader, context for the whole app
data AppData = AppData { appConnPool :: Pool PGS.Connection }

type AppStackM = ReaderT AppData Handler


--------------------------------------------------
-- | Db Setup

-- | Helper Fn
printSql :: Default Unpackspec a a => Query a -> IO ()
printSql = putStrLn . maybe "Empty query" id . showSqlForPostgres

-- | Db Records
type DbUserColumns = '[CEmail, CAge]
type DbPasswordColumns = '[CEmail, CHashPassword]
type DbPassword = '[FEmail, FHashPassword]

userTable :: Table (Record DbUserColumns) (Record DbUserColumns)
userTable = Table "account" defaultRecTable

passwordTable :: Table (Record DbPasswordColumns) (Record DbPasswordColumns)
passwordTable = Table "password" defaultRecTable

withDb :: (MonadBaseControl IO m, MonadReader AppData m)
       => (PGS.Connection -> IO a)
       -> m a
withDb action = do
  pool <- asks appConnPool
  liftBase $ withResource pool action


--------------------------------------------------
-- | Other Records

type Account = '[FEmail, FAge]

-- Create: newtype ApiAccount = ApiAccount {unApiAccount :: Record Account}
makeRecJsonWrapper "ApiAccount" ''Account

-- 
makeWrapped ''ApiAccount

-- | Client-submitted credentials for Authentication
type AuthAccount = '[FEmail, FClearPassword]

-- Create: newtype ApiAuthAccount = ApiAuthAccount {unApiAuthAccount :: Record AuthAccount}
makeRecJsonWrapper "ApiAuthAccount" ''AuthAccount

-- 
makeWrapped ''ApiAuthAccount


--------------------------------------------------
-- General Query

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
-- | Security
-- note: you probably want better security than this

hashPassword :: ClearPassword Text -> Text
hashPassword (ClearPassword "secret") = "garbledHash"
hashPassword _ = "hashOfNotSecret"

validatePassword :: ClearPassword Text -> Text -> Bool
validatePassword clear hash = (hashPassword clear) == hash


--------------------------------------------------
-- | Servant

type LoginRoute = ReqBody '[JSON] ApiAuthAccount
                  :> Post '[JSON] ApiAccount

-- TODO: refactor unsafe fns, return 403's instead
-- TODO: refactor runQuery
-- TODO: execute in Maybe monad
loginRoute :: ApiAuthAccount -> AppStackM ApiAccount
loginRoute apiAuthAccount = do
  let authAccount = (unApiAuthAccount apiAuthAccount)
      email = view fEmail authAccount

  -- grab password to validate against
  passwords <- withDb $ \conn ->
    runQuery conn $ proc () -> do
      ps <- queryByEmail passwordTable email -< ()
      returnA -< ps
  let _ = passwords :: [Record DbPassword] -- helps type system

  -- validate provided credentials
  let passwordEntity = head passwords -- TODO: head unsafe
      clearPassword = view fClearPassword authAccount
      hash = view fHashPassword passwordEntity
      valid = validatePassword clearPassword hash
      ret = case valid of
        True -> do

          -- return the appropriate user
          users <- withDb $ \conn ->
            runQuery conn $ proc () -> do
              us <- queryByEmail userTable email -< ()
              returnA -< us
          let user = head users -- TODO: head unsafe
          pure $ ApiAccount user
        False -> throwError err403 
  ret

type Api = "login" :> LoginRoute
  
api :: Proxy Api
api  = Proxy

service :: ServerT Api AppStackM
service = loginRoute


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
  withPostgresqlPool "host=localhost port=5432 user=god dbname=opaleyesandbox password=godpass" 2
    $ \connPool -> do
      let appData = AppData connPool
      run 8080 $ serve api (appServer appData)

appServer :: AppData -> Server Api
appServer appData = enter (appStackToHandler appData) service
      
appStackToHandler' :: forall a. AppData -> AppStackM a -> Handler a
appStackToHandler' appData action = runReaderT action appData

appStackToHandler :: AppData -> (AppStackM Servant.:~> Handler)
appStackToHandler appData = Nat $ appStackToHandler' appData


{-

# SQL

CREATE DATABASE opaleyesandbox;
CREATE USER god;
grant all privileges on database opaleyesandbox to god;

CREATE TABLE account (
  email Text PRIMARY KEY,
  age Integer
);

ALTER TABLE account owner to god;

CREATE TABLE password (
  email Text PRIMARY KEY,
  hashpass Text NOT NULL
);

ALTER TABLE password owner to god;

INSERT INTO user (email, age) VALUES
('a', 1);

INSERT INTO password (email, hashpass) VALUES
('a', 'garbledHash')

--------------------------------------------------

# Curl test

curl -i -H 'Content-Type: application/json' -XPOST 'http://localhost:8080/login' -d '{
"email":"a",
"clearpass":"secret"
}'


-}
