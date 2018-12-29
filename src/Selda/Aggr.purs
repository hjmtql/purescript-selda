module Selda.Aggr
  ( Aggr(..)
  , WrapWithAggr(..)
  , UnAggr(..)
  ) where

import Heterogeneous.Mapping (class Mapping)
import Selda.Col (Col)

newtype Aggr s v a = Aggr (Col s v a)

data WrapWithAggr = WrapWithAggr
instance wrapWithAggrInstance
    ∷ Mapping WrapWithAggr (Col s v a) (Aggr s v a)
  where
  mapping _ = Aggr

data UnAggr = UnAggr
instance unAggrInstance ∷ Mapping UnAggr (Aggr s v a) (Col s v a) where
  mapping _ (Aggr col) = col
