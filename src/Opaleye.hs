{- stack
--resolver lts-8.2
--install-ghc
exec ghci
--package opaleye
--package postgresql-simple
--package profunctor
--package arrow
--package text
-}

{-|

Following along with one of Opaleye's many great tutorials in their
repo:

# Tutorial Basic
https://github.com/tomjaguarpaw/haskell-opaleye/blob/master/Doc/Tutorial/TutorialBasic.lhs

-}

{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}


import           Prelude hiding (sum)
import           Opaleye (Column, Nullable, matchNullable, isNull,
                         Table(Table), required, queryTable,
                         Query, QueryArr, restrict, (.==), (.<=), (.&&), (.<),
                         (.===),
                         (.++), ifThenElse, pgString, aggregate, groupBy,
                         count, avg, sum, leftJoin, runQuery,
                         showSqlForPostgres, Unpackspec,
                         PGInt4, PGInt8, PGText, PGDate, PGFloat8, PGBool)
import           Data.Profunctor.Product (p2, p3)
import           Data.Profunctor.Product.Default (Default)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Time.Calendar (Day)
import           Control.Arrow (returnA)

import qualified Database.PostgreSQL.Simple as PGS

----------
-- Helper Fn

printSql :: Default Unpackspec a a => Query a -> IO ()
printSql = putStrLn . maybe "Empty query" id . showSqlForPostgres


----------
-- Basic Table & Query
type PersonRead = (Column PGText, Column PGInt4, Column PGText)
type PersonWrite = (Column PGText, Column PGInt4, Column PGText)

personTable :: Table PersonRead PersonWrite
personTable = Table "personTable" (p3 (required "name"
                                      , required "age"
                                      , required "address"))

personQuery :: Query PersonRead
personQuery = queryTable personTable


----------
-- Custom Record & TH

data Birthday' a b = Birthday { bdName :: a , bdDay :: b }
type Birthday = Birthday' String Day
type BirthdayColumn = Birthday' (Column PGText) (Column PGDate)

$(makeAdaptorAndInstance "pBirthday" ''Birthday') -- instances for user defined type

birthdayTable :: Table BirthdayColumn BirthdayColumn
birthdayTable = Table "birthdayTable"
  (pBirthday Birthday { bdName = required "name"
                      , bdDay = required "birthday" })

birthdayQuery :: Query BirthdayColumn
birthdayQuery = queryTable birthdayTable


----------
-- Projection

nameAge :: Query (Column PGText, Column PGInt4)
nameAge = proc () -> do
  (name, age, _) <- personQuery -< ()
  returnA -< (name, age)


----------
-- Product

personBirthdayProduct :: Query (PersonRead, BirthdayColumn)
personBirthdayProduct = proc () -> do
  personRow <- personQuery -< ()
  birthdayRow <- birthdayQuery -< ()
  returnA -< (personRow, birthdayRow)


----------
-- Restriction

youngPeople :: Query PersonRead
youngPeople = proc () -> do
  row@(_, age, _) <- personQuery -< ()
  restrict -< age .<= 18
  returnA -< row

twentiesAtAddress :: Query PersonRead
twentiesAtAddress = proc () -> do
  row@(_, age, address) <- personQuery -< ()
  restrict -< (20 .<= age) .&& (age .< 30)
  restrict -< address .== pgString "123 Sesame st."
  returnA -< row

  
----------
-- Inner Join

personAndBirthday :: Query (Column PGText, Column PGInt4, Column PGText, Column PGDate)
personAndBirthday = proc () -> do
  (name, age, address) <- personQuery -< ()
  birthday <- birthdayQuery -< ()
  restrict -< name .== bdName birthday
  returnA -< (name, age, address, bdDay birthday)


----------
-- Nullability

type EmployeeRead = (Column PGText, Column (Nullable PGText))
type EmployeeWrite = (Column PGText, Column (Nullable PGText))

employeeTable :: Table EmployeeRead EmployeeWrite
employeeTable = Table "employeeTable" (p2 (required "name",
                                          required "boss"))

hasBoss :: Query (Column PGText)
hasBoss = proc () -> do
  (name, nullableBoss) <- queryTable employeeTable -< ()
  let aOrNo = ifThenElse (isNull nullableBoss) (pgString "no") (pgString "a")
  returnA -< name .++ pgString " has " .++ aOrNo .++ pgString " boss"

bossQuery :: QueryArr (Column PGText, Column (Nullable PGText)) (Column PGText)
bossQuery = proc (name, nullableBoss) -> do
  returnA -< matchNullable (name .++ pgString " has no boss")
                           (\boss -> pgString "The boss of " .++ name
                                     .++ pgString " is " .++ boss)
                           nullableBoss

----------
-- Composability

restrictIsTwenties :: QueryArr (Column PGInt4) ()
restrictIsTwenties = proc age -> do
  restrict -< (20 .<= age) .&& (age .< 30)

restrictAdderssIs123Sesame :: QueryArr (Column PGText) ()
restrictAdderssIs123Sesame = proc address -> do
  restrict -< address .== pgString "123 Sesame St."

twentiesAtAddress' :: Query PersonRead
twentiesAtAddress' = proc () -> do
  row@(_, age, address) <- personQuery -< ()
  restrictIsTwenties -< age
  restrictAdderssIs123Sesame -< address
  returnA -< row


----------
-- Composability of Joins

birthdayOfPerson :: QueryArr (Column PGText) (Column PGDate)
birthdayOfPerson = proc name -> do
  birthday <- birthdayQuery -< ()
  restrict -< name .== bdName birthday
  returnA -< bdDay birthday

personAndBirthday' :: Query (Column PGText, Column PGInt4, Column PGText, Column PGDate)
personAndBirthday' = proc () -> do
  (name, age, address) <- personQuery -< ()
  birthday <- birthdayOfPerson -< name
  returnA -< (name, age, address, birthday)


----------
-- Aggregation

data Widget a b c d e = Widget { style :: a
                               , color :: b
                               , location :: c
                               , quantity :: d
                               , radius :: e}

$(makeAdaptorAndInstance "pWidget" ''Widget)

widgetTable :: Table (Widget (Column PGText) (Column PGText) (Column PGText)
                             (Column PGInt4) (Column PGFloat8))
                     (Widget (Column PGText) (Column PGText) (Column PGText)
                             (Column PGInt4) (Column PGFloat8))
widgetTable = Table "widgetTable"
                     (pWidget Widget { style    = required "style"
                                     , color    = required "color"
                                     , location = required "location"
                                     , quantity = required "quantity"
                                     , radius   = required "radius" })

aggregateWidgets :: Query (Widget (Column PGText) (Column PGText) (Column PGInt8)
                                  (Column PGInt4) (Column PGFloat8))
aggregateWidgets = aggregate (pWidget (Widget { style    = groupBy
                                              , color    = groupBy
                                              , location = count
                                              , quantity = sum
                                              , radius   = avg }))
                             (queryTable widgetTable)


----------
-- Outer Join
--   note that because of typeclass "magic" behind the scenes, you may
--   need to specify the types of an outer join very explicitly

type ColumnNullableBirthday = Birthday' (Column (Nullable PGText))
                              (Column (Nullable PGDate))

personBirthdayLeftJoin :: Query ((Column PGText, Column PGInt4, Column PGText),
                                 ColumnNullableBirthday)
personBirthdayLeftJoin = leftJoin personQuery birthdayQuery eqName
    where eqName ((name, _, _), birthdayRow) = name .== bdName birthdayRow


----------
-- Newtypes for more safety
--   ex: so you don't accidentally compare ids that are Ints to
--   inventory counts that are Ints

data Warehouse' a b c = Warehouse { wId       :: a
                                  , wLocation :: b
                                  , wNumGoods :: c }
                        
$(makeAdaptorAndInstance "pWarehouse" ''Warehouse')

newtype WarehouseId' a = WarehouseId a

$(makeAdaptorAndInstance "pWarehouseId" ''WarehouseId')

type WarehouseIdColumn = WarehouseId' (Column PGInt4)
type WarehouseColumn = Warehouse' WarehouseIdColumn
                       (Column PGText)
                       (Column PGInt4)

warehouseTable :: Table WarehouseColumn WarehouseColumn
warehouseTable = Table "warehouse_table"
  (pWarehouse Warehouse { wId = pWarehouseId (WarehouseId (required "id"))
                        , wLocation = required "location"
                        , wNumGoods = required "num_goods" })

-- forbiddenComparison :: WarehouseColumn -> Column PGBool
-- forbiddenComparison w = wId w .== wNumGoods w

permittedComparison :: WarehouseColumn
                    -> WarehouseColumn
                    -> Column PGBool
permittedComparison w1 w2 = wId w1 .=== wId w2


----------
-- Running Queries
--   make sure to include type sigs for clean errors

runEmployeesQuery :: PGS.Connection
                  -> Query (Column PGText, Column (Nullable PGText))
                  -> IO [(String, Maybe String)]
runEmployeesQuery = runQuery

type WarehouseId = WarehouseId' Int
type Warehouse = Warehouse' WarehouseId String Int

runWarehouseQuery :: PGS.Connection
                  -> Query WarehouseColumn
                  -> IO [Warehouse]
runWarehouseQuery = runQuery
