{-# LANGUAGE InstanceSigs #-}

{- | Basic IL datatypes

The grammar is based on [A Hierarchy of Program Transformers](https://www.researchgate.net/publication/229062264_A_Hierarchy_of_Program_Transformers).
-}
module Ast (
    Expr (Var, Con, Lam, Fun, App, Case, Let),
    Fun (FunName, funName),
    Var (VarName, varName),
    Con (ConName, conName),
    Alt (Alt),
    Alts,
) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

import Text.PrettyPrint.Leijen.Text (
    Doc,
    Pretty,
    align,
    hsep,
    parens,
    pretty,
    text,
    textStrict,
    vsep,
    (<+>),
 )

-- | Function name, must be lowercase
newtype Fun = FunName {funName :: Text}
    deriving (Show, Eq, Ord)

instance Pretty Fun where
    pretty :: Fun -> Doc
    pretty (FunName fun) = textStrict fun

-- | Variable name, must be lowercase
newtype Var = VarName {varName :: Text}
    deriving (Show)

instance Pretty Var where
    pretty :: Var -> Doc
    pretty (VarName var) = textStrict var

-- | Constructor name, must start with uppercase letter
newtype Con = ConName {conName :: Text}
    deriving (Show, Ord, Eq)

instance Pretty Con where
    pretty :: Con -> Doc
    pretty (ConName con) = textStrict con

-- | Case pattern-matching alternatives map
type Alts = Map Con Alt

instance Pretty Alts where
    pretty :: Alts -> Doc
    pretty alts = vsep $ map (\(con, alt) -> pretty con <+> pretty alt) list
      where
        list = Map.toList alts

-- | Case pattern-matching alternative
data Alt = Alt [Var] Expr
    deriving (Show)

instance Pretty Alt where
    pretty :: Alt -> Doc
    pretty (Alt vars expr) = hsep (fmap pretty vars) <+> text "=>" <+> pretty expr

-- | IL expression type
data Expr
    = -- | Variable
      Var Var
    | -- | Sum type constructor with arbitrary expression inside
      Con Con [Expr]
    | -- | Lambda abstraction
      Lam Var Expr
    | -- | Function call
      Fun Fun
    | -- | Expressions' application
      App Expr Expr
    | -- | Pattern-matching using case, patterns must be non-overlapping and exhaustive
      Case Expr Alts
    | -- | Simple let expression, can be introduced only by distillation
      Let Var Expr Expr
    deriving (Show)

{- | PPrinting currently is very simple and only used for debugging.
Most probably will change after source language design.
-}
instance Pretty Expr where
    pretty :: Expr -> Doc
    pretty (Var var) = pretty var
    pretty (Con con exprs) = pretty con <+> hsep (fmap pretty exprs)
    pretty (Lam var expr) = text "\\" <> pretty var <> text "." <> pretty expr
    pretty (Fun fun) = pretty fun
    pretty (App expr1 expr2) = parens (pretty expr1) <+> parens (pretty expr2)
    pretty (Case expr alts) = text "case" <+> pretty expr <+> text "of" <+> align (pretty alts)
    pretty (Let var varExpr expr) = text "let" <+> pretty var <+> text "=" <+> pretty varExpr <+> text "in" <+> pretty expr
