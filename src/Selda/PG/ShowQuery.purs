module Selda.PG.ShowQuery where

import Prelude

import Data.Array (foldl, reverse)
import Data.Array as Array
import Data.Exists (Exists)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant)
import Selda.Expr (showExpr)
import Selda.Query.Type (GenState, Order(..), SQL(..), Source(..))
import Selda.Table (Alias, Column(..))

type Base = 
  ( boolean ∷ Boolean
  , string ∷ String
  , int ∷ Int
  , null ∷ Unit
  , column ∷ Exists Column
  )

showState ∷ GenState Base → String
showState { cols, sources, restricts, aggr, order, limit } = 
  showCols cols
    <> showSources sources
    <> showRestricts restricts
    <> showGrouping aggr
    <> showOrdering order
    <> showLimit limit

showCols ∷ Array (Tuple Alias (Variant Base)) → String
showCols = case _ of
  [] → ""
  xs → "SELECT " <> (joinWith ", " $ map showAliasedCol xs)

showSources ∷ Array (Source Base) → String
showSources sources = case Array.uncons $ reverse sources of
  Nothing → ""
  Just { head, tail } → " FROM "
    <> foldl (\acc x → acc <> sepFor x <> showSource x) (showSource head) tail
  -- Just { head: h@(Product t), tail } →
  --   " FROM " <> foldl (\acc x → acc <> sepFor x <> showSource x) (showSource h) tail
  -- Just { head: LeftJoin t _, tail } →
  --   -- join on the first place, drop it and interpret as Product
  --   showSources $ Product t : tail

showRestricts ∷ Array (Variant Base) → String
showRestricts = case _ of
  [] → ""
  xs → " WHERE " <> (joinWith " AND " $ map (\e → "(" <> showExpr e <> ")") xs)

showGrouping ∷ Array (Variant Base) → String
showGrouping = case _ of
  [] → ""
  xs → " GROUP BY " <> (joinWith ", " $ map showExpr xs)

showOrdering ∷ Array (Tuple Order (Variant Base)) → String
showOrdering = case _ of
  [] → ""
  xs → " ORDER BY " <> (joinWith ", " $ map showOrder xs)

showOrder ∷ Tuple Order (Variant Base) → String
showOrder (Tuple order e) =
  showExpr e <> " "
    <> case order of
      Asc → "ASC"
      Desc → "DESC"

showLimit ∷ Maybe Int → String
showLimit = case _ of
  Nothing → ""
  Just i → " LIMIT " <> (show $ max 0 i)

showSQL ∷ SQL Base → String
showSQL = case _ of
  FromTable t →
    t.name <> " " <> t.alias
  SubQuery alias state → 
    "(" <> showState state <> ") " <> alias

sepFor ∷ Source Base → String
sepFor = case _ of
  Product _ → ", "
  LeftJoin _ _ → " LEFT JOIN "

showSource ∷ Source Base → String
showSource = case _ of
  Product t → showSQL t
  LeftJoin t e → showSQL t <> " ON (" <> showExpr e <> ")"

showAliasedCol ∷ Tuple Alias (Variant Base) → String
showAliasedCol (Tuple alias ee) = showExpr ee <> " AS " <> alias
