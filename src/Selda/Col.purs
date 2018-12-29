module Selda.Col where

import Prelude

import Data.Array ((:))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, inj)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Heterogeneous.Mapping (class HMap, class Mapping, hmap)
import Prim.Row (class Cons)
import Prim.RowList (kind RowList)
import Selda.Expr (_column)
import Selda.Table (Alias, Column)
import Type.Proxy (Proxy)
import Unsafe.Coerce (unsafeCoerce)

newtype Col s v a = Col (Variant v)
derive instance newtypeCol ∷ Newtype (Col s v a) _

-- showCol ∷ ∀ s a. Col s a → String
-- showCol = unwrap >>> showExpr

class IsSymbol sym <= TypeToSym a sym | a → sym, sym → a

instance typeToSymInt ∷ TypeToSym Int "int"
else instance typeToSymString ∷ TypeToSym String "string"
else instance typeToSymBoolean ∷ TypeToSym Boolean "boolean"
else instance typeToSymMaybe ∷ TypeToSym (Maybe a) "null"
-- else instance typeToSymMaybe ∷ TypeToSym a sym ⇒ TypeToSym (Maybe a) sym

class Lit a v | a → v where
  lit ∷ ∀ s. a → Col s v a

instance litMaybe
    ∷ ( Lit a v
      , TypeToSym (Maybe a) sym
      , Cons sym Unit v v'
      )
      ⇒ Lit (Maybe a) v'
  where
  lit = case _ of
    Nothing → Col $ inj (SProxy ∷ SProxy sym) unit
    Just l → 
      let expand = (unsafeCoerce ∷ Variant v → Variant v') in
      Col $ expand $ unwrap $ lit l
else instance litA ∷ (TypeToSym a sym, Cons sym a v v') ⇒ Lit a v' where
  lit = Col <<< inj (SProxy ∷ SProxy sym)

example ∷ Unit
example = unit
  where
  a = lit "dsads"  ∷ Col _ ( string ∷ String | _ ) String
  b = lit false    ∷ Col _ ( boolean ∷ Boolean | _ ) Boolean
  c = lit 1        ∷ Col _ ( int ∷ Int | _ ) Int
  d = lit (Just 1) ∷ Col _ ( int ∷ Int, null ∷ Unit | _ ) (Maybe Int)

-- | ```purescript
-- | { name ∷ Column String, id ∷ Column Int }
-- | → 
-- | { name ∷ Col s String, id ∷ Col s Int }
-- | ```
class ToCols s i o | s i → o where
  toCols ∷ Proxy s → { | i } → { | o }

instance toColsI ∷ HMap (ToCols_ s) { | i } { | o } ⇒ ToCols s i o where
  toCols _ = hmap (ToCols_ ∷ ToCols_ s)

data ToCols_ s = ToCols_
instance toColsMapping
    ∷ Mapping (ToCols_ s) (Column a) (Col s ( column ∷ Column a | v ) a)
  where
  mapping _ col = Col $ inj _column col

-- | For record { n1 ∷ Col s String, n2 ∷ Col s String, id ∷ Col s Int }
-- | → [(id, Expr Int), (n1, Expr String), (n2, Expr String)]
-- | → [(id, Exists Expr), (n1, Exists Expr), (n2, Exists Expr)]
class GetCols r v | r → v where
  getCols ∷ { | r } → Array (Tuple Alias (Variant v))
instance getcols 
    ∷ HFoldlWithIndex ExtractCols 
      (Array (Tuple String (Variant v)))
      { | r }
      (Array (Tuple String (Variant v))) 
    ⇒ GetCols r v
  where
  getCols r = hfoldlWithIndex ExtractCols ([] ∷ Array (Tuple String (Variant v))) r

data ExtractCols = ExtractCols
instance extractcols 
    ∷ IsSymbol sym 
    ⇒ FoldingWithIndex ExtractCols (SProxy sym) 
      (Array (Tuple String (Variant v)))
      (Col s v a) 
      (Array (Tuple String (Variant v)))
  where
  foldingWithIndex ExtractCols sym acc (Col e) = 
    Tuple (reflectSymbol (SProxy ∷ SProxy sym)) e : acc
