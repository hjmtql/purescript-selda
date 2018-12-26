module Selda
  ( module Query.Type
  , module Col
  , module PG
  , module Query
  , module Table
  , (.==), expEq
  , (.>), expGt
  -- , (.<)
  -- , (.>=)
  -- , (.<=)
  -- , (.&&)
  , (.||), expOr
  , count
  , max_
  , asc, desc
  ) where

import Prelude

import Selda.Aggr (Aggr(..))
import Selda.Col (Col(..))
import Selda.Col (Col(..), lit, class Lit) as Col
import Selda.Expr (BinOp(..), Expr(..), Fn(..))
import Selda.PG (withPG, query, insert_, insert, deleteFrom, update) as PG
import Selda.Query (crossJoin, crossJoin_, restrict, leftJoin, leftJoin_, aggregate, groupBy, groupBy', selectFrom, selectFrom_, limit, orderBy) as Query
import Selda.Query.Type (Order(..))
import Selda.Query.Type (Query(..), FullQuery(..)) as Query.Type
import Selda.Table (Table(..)) as Table

expOr ∷ ∀ s. Col s Boolean → Col s Boolean → Col s Boolean
expOr (Col e1) (Col e2) = Col $ EBinOp Or e1 e2

expGt ∷ ∀ s a. Col s a → Col s a → Col s Boolean
expGt (Col e1) (Col e2) = Col $ EBinOp Gt e1 e2

expEq ∷ ∀ s a. Col s a → Col s a → Col s Boolean
expEq (Col e1) (Col e2) = Col $ EBinOp Eq e1 e2

count ∷ ∀ s a. Col s a → Aggr s String
count (Col e) = Aggr $ Col $ EFn $ FnCount e

max_ ∷ ∀ s a. Col s a → Aggr s a
max_ (Col e) = Aggr $ Col $ EFn $ FnMax e

asc ∷ Order
asc = Asc

desc ∷ Order
desc = Desc

-- instance colHeytingAlgebra ∷ HeytingAlgebra (Col s Boolean) where

-- infixl 4 `like`
infixl 4 expEq as .==
infixl 4 expGt as .>
-- infixl 4 expLt as .<
-- infixl 4 expGe as .>=
-- infixl 4 expLe as .<=
-- infixr 3 expAnd as .&&
infixr 2 expOr as .||
