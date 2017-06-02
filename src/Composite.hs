{- stack
--resolver lts-8.2
--install-ghc
exec ghci
--package aeson
--package composite-base
--package composite-aeson
--package text
-}

{-# LANGUAGE
DataKinds
, OverloadedStrings
, PatternSynonyms
, TypeOperators #-}

import qualified Data.Aeson as Aeson
import Composite.Aeson (JsonFormat, defaultJsonFormatRec, recJsonFormat, toJsonWithFormat)
import Composite.Record (Record, Rec(RNil), (:->), pattern (:*:))
import Data.Text (Text)

type FId   = "id"   :-> Int
type FName = "name" :-> Text
type User = '[FId, FName]

userFormat :: JsonFormat e (Record User)
userFormat = recJsonFormat defaultJsonFormatRec

alice :: Record User
alice = 1 :*: "Alice" :*: RNil

aliceJson :: Aeson.Value
aliceJson = toJsonWithFormat userFormat alice
