module Selda.Expr where

import Prelude

import Data.Exists (Exists, runExists)
import Prim.RowList (kind RowList)
import Selda.Table (Column, showColumn)

data Literal
  = LBoolean Boolean
  | LString String
  | LInt Int
  | LNull
  | LJust Literal

data BinOp
  = Or
  | Gt
  | Eq

data Expr
  = EColumn (Exists Column)
  | ELit Literal
  | EBinOp BinOp Expr Expr
  | EFn Fn

data Fn
  = FnMax Expr
  | FnCount Expr

showLiteral ∷ Literal → String
showLiteral = case _ of
  LBoolean b  → show b
  LString s  → "'" <> s <> "'"
  LInt i  → show i
  LNull → "null"
  LJust x → showLiteral x

showBinOp ∷ BinOp → String
showBinOp = case _ of
  Or → " || "
  Gt → " > "
  Eq → " = "

showExpr ∷ Expr → String
showExpr = case _ of
  EColumn col → runExists showColumn col
  ELit lit → showLiteral lit
  EBinOp op e1 e2 → "(" <> showExpr e1 <> showBinOp op <> showExpr e2 <> ")"
  EFn fn → showFn fn

showFn ∷ Fn → String
showFn = case _ of
  FnMax e → "max(" <> showExpr e <> ")"
  FnCount ee → "count(" <> showExpr ee <> ")"
