{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}


-- TODO: This made a `state/` dir that is prob safe to delete IF you
-- don't have anything important in the acid db

module Acid where

import Data.Acid
import Data.Typeable
import qualified Data.IntMap as IntMap
import Control.Monad.Reader
import Control.Monad.State
import Data.SafeCopy

data Person = Person {name::String, favNum::Int}
  deriving (Show, Typeable)

data Db = Db { allPersons :: IntMap.IntMap Person }
  deriving (Typeable)


-- 
everyone :: Query Db [Person]
everyone = IntMap.elems . allPersons <$> ask

addPerson :: Person -> Update Db ()
addPerson p = modify go
  where
    go (Db d) = Db $
      case IntMap.maxViewWithKey d of
        Just ((max, _), _) -> IntMap.insert (max+1) p d
        Nothing -> IntMap.singleton 1 p

deriveSafeCopy 0 'base ''Person
deriveSafeCopy 0 'base ''Db
makeAcidic ''Db ['everyone, 'addPerson]


main = do
  state <- openLocalState (Db IntMap.empty)
  update state (AddPerson $ Person "adam" 42)
  update state (AddPerson $ Person "bill" 152)
  db <- query state Everyone
  mapM_ print db


{-

Notes:
- Typeable?
- SafeCopy?
- acid-state without TH?

-}
