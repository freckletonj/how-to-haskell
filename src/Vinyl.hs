{- stack
--resolver lts-8.2
--install-ghc
exec ghci
--package aeson
--package vinyl
--package lens
--package singletons
--package text
--package composite-base-0.4.1.0
--package composite-aeson-0.4.1.0
--package composite-opaleye-0.4.1.0
-}






{-# LANGUAGE DataKinds #-} -- converts sum constructors to types
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}


import Data.Vinyl
import Data.Vinyl.Functor
import Data.Vinyl.TypeLevel

import qualified Data.Aeson as Aeson
import Composite.Aeson (JsonFormat, defaultJsonFormatRec, recJsonFormat, toJsonWithFormat)
import Composite.Record (Record, Rec(RNil), (:->), pattern (:*:))

import Control.Applicative
import Control.Lens hiding (Identity, rmap)
import Control.Lens.TH
import Data.Char
import Data.Singletons.TH
import Data.Maybe
import Data.Text (Text)

{-

# How to Run

```
stack src/Vinyl.hs
```

# TODO

* Fields - (one, many) * (one, many)


# Definitions

* User in Db:
  email :: Email
  name  :: Name

* Password in Db:
  emailFk :: Email -- also encode Persistent FK?
  hash    :: HashPass

* FavoriteColor in Db:
  email :: Email
  color :: Color

* User Signup/Login Creds From Client:
  email     :: Email
  clearPass :: ClearPass

* User sent to Client:
  email :: Email
  <other traits that have been JOINed in?>



-}

--------------------------------------------------
-- |


data Fields = Email | Name | HashPass | ClearPass | Friend
  deriving Show

type DbUser = [Email, Name]
type DbPass = [Email, HashPass]
type AuthUser = [Email, ClearPass]
type ClientUser = [Email, Name, Friend]

type family ElF (f :: Fields) :: * where
  ElF Email = String
  ElF Name = String
  ElF HashPass = String
  ElF ClearPass = String
  ElF Friend = Rec Attr DbUser

newtype Attr f = Attr { _unAttr :: ElF f }
makeLenses ''Attr

--------------------------------------------------
-- | Show

instance Show (Attr Email) where show (Attr x) = show x
instance Show (Attr Name) where show (Attr x) = show x
instance Show (Attr HashPass) where show (Attr x) = show x
instance Show (Attr ClearPass) where show (Attr x) = show x -- TODO: don't show
instance Show (Attr Friend) where show (Attr x) = show x

--------------------------------------------------
-- | Value setter

(=::) :: sing f -> ElF f -> Attr f
_ =:: x = Attr x

genSingletons [ ''Fields ]

--------------------------------------------------
-- Implementation

--adam :: Rec Attr DbUser
adam = (SName =:: "adam")
       :& (SEmail =:: "adam@nowhere.com")
       :& (SFriend =:: bryan)
       :& RNil

bryan :: Rec Attr DbUser
bryan = (SEmail =:: "bryan@nowhere.com")
        :& (SName =:: "bryan")
        :& RNil

adamAppendFriend = adam <+> bryan -- weird
adamsFriend = rget SFriend adam


--------------------------------------------------
-- | Validation

type Validator f = Lift (->) f (Maybe :. f)

validateEmail :: Attr Email -> Maybe (Attr Email)
validateEmail (Attr str) | (str :: ElF Email) == "bademail" = Nothing
validateEmail (Attr str) = Just (Attr str)


validateClearPass (Attr str) = Just (Attr str)

vAuthUser :: Rec (Validator Attr) AuthUser
vAuthUser = lift validateEmail
              :& lift validateClearPass
              :& RNil
  where
    lift f = Lift $ Compose . f

--------------------------------------------------
-- Implementation

catherineCreds :: Rec Attr AuthUser
catherineCreds = (SEmail =:: "catherine@gmail.com")
                 :& (SClearPass =:: "super-secret")
                 :& RNil

vCatherine :: Rec (Maybe :. Attr) AuthUser
vCatherine = vAuthUser <<*>> catherineCreds

badCreds :: Rec Attr AuthUser
badCreds = (SEmail =:: "bademail")
           :& (SClearPass =:: "whatevs")
           :& RNil

vBad = vAuthUser <<*>> badCreds

{-
λ> getCompose $ vBad ^. rlens SEmail
Nothing

λ> getCompose $ vCatherine ^. rlens SEmail
Just "catherine@gmail.com"

λ> rtraverse getCompose vCatherine
Just {"catherine@gmail.com", "super-secret"}

λ> rtraverse getCompose vBad
Nothing
-}


--------------------------------------------------
-- rmap

type Mapper f = Lift (->) f f

wowEmail :: Attr Email -> Attr Email
wowEmail (Attr x) = Attr $ x ++ "!"

-- wowDbUser :: Rec ((->) (Rec Attr DbUser)) DbUser 
-- wowDbUser = undefined


-- rmapAdam = rmap wowDbUser adam

