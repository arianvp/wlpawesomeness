{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Language
  ( Name
  , Variable(..)
  , Statement(..)
  , Expression(..)
  , PrimitiveType(..)
  , Array(..)
  , Type(..)
  , BinOp(..)
  , Quantifier(..)
  , AsgTarget(..)
  , forAll
  , exists
  , (+.)
  , pattern (:+:)
  , (-.)
  , pattern (:-:)
  , (&&.)
  , pattern (:&&:)
  , (||.)
  , pattern (:||:)
  , (==>.)
  , pattern (:==>:)
  , (<.)
  , pattern (:<:)
  , (<=.)
  , pattern (:<=:)
  , (=.)
  , pattern (:=:)
  , int
  , bool
  , showStmts
  ) where

import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.Generics.Uniplate.Data ()
import Text.PrettyPrint hiding (int)
import GHC.Generics

data AsgTarget
  = N Name
  | A Name Expression
  deriving  (Eq, Data, Typeable, Generic)

instance Show AsgTarget where
  show (N name) = name
  show (A name expr) = name ++ "[" ++ show expr ++ "]"

data Statement
  = Skip
  | Assert Expression
  | Assume Expression
  | (:=) AsgTarget
         Expression
  | If Expression
       [Statement]
       [Statement]
  | While Expression
          [Statement]
  | Var [Variable]
        [Statement]
  deriving (Eq, Data, Typeable, Generic)


instance Show Statement where
  show x = render (stmt x)

instance {-#OVERLAPS#-} Show [Statement] where
  show = showStmts
showStmts :: [Statement] -> String
showStmts = render . stmts

stmts :: [Statement] -> Doc
stmts ss = vcat $ punctuate (text ";") (map stmt ss)

stmt :: Statement -> Doc
stmt Skip = text "skip"
stmt (Assert expr) = text "assert " <> text (show expr)
stmt (Assume expr) = text "assume " <> text (show expr)
stmt (If e s1 s2) =
  text "if(" <> text (show e) <> text "){" $$ nest 2 (stmts s1) $$
  text "} else {" $$
  nest 2 (stmts s2) $$
  text "}"
stmt (While e s) =
  text "while(" <> text (show e) <> text "){" $$ nest 2 (stmts s) $$ text "}"
stmt (a := b) =  text (show a) <> text " := " <> text (show b)
stmt (Var vars s) =
  text "var(" <> (hcat $ punctuate (text ",") (map (text . show) vars)) <>
  text "){" $$
  nest 2 (stmts s) $$
  text "}"

type Name = String


data Variable =
  Variable Name
           Type
  deriving (Eq, Data, Typeable, Generic)

instance Ord Variable where
  compare (Variable _ t) (Variable _ t') = compare t t'

instance Show Variable where
  show (Variable name typ) = name ++ ":" ++ show typ

data BinOp
  = Plus
  | Min
  | Conj
  | Disj
  | Impl
  | Le
  | Leq
  | Eq
  deriving (Eq, Data, Typeable, Ord, Generic)


instance Show BinOp where
  show x =
    case x of
      Plus -> "+"
      Min -> "-"
      Conj -> "∧"
      Disj -> "∨"
      Impl -> "⇒"
      Le -> "<"
      Leq -> "≤"
      Eq -> "="
  -- todo fixitivity

data Quantifier
  = Exists Variable
  | ForAll Variable
  deriving (Eq, Ord, Data, Typeable, Generic)

instance Show Quantifier where
  show q =
    case q of
      (ForAll var) -> "∀" ++ show var
      (Exists var) -> "∃" ++ show var



forAll :: Variable -> Expression -> Expression
forAll v e = Quantified (ForAll v) e

exists :: Variable -> Expression -> Expression
exists v e = Quantified (Exists v) e

data Expression
  = IntVal Int
  | BoolVal Bool
  | Name String
  | BinOp BinOp
          Expression
          Expression
  | Quantified Quantifier Expression
  | Not Expression
  | ArrayAt Expression
            Expression
  | ProgramCall Name [Expression]
  | IfThenElseE Expression Expression Expression
  deriving (Eq, Data, Typeable, Ord, Generic)





instance Show Expression where
  show x =
    case x of
      IntVal i -> show i
      BoolVal b -> show b
      Name s -> s
      BinOp binOp e1 e2 -> "(" ++ show e1 ++ show binOp ++ show e2 ++ ")"
      Quantified (ForAll var) e -> "∀" ++ show var ++ "." ++ show e
      Quantified (Exists var) e -> "∃" ++ show var ++ "." ++ show e

      Not e -> "¬" ++ show e
      ArrayAt n e -> show n ++ "[" ++ show e ++ "]"
      ProgramCall name args -> name ++ "(" ++  render (hcat $ punctuate (text ",") (map (text . show) args)) ++ ")"
      IfThenElseE pred' left right -> show pred' ++ "->" ++ "(" ++ show left ++ ")" ++ "|" ++ "(" ++ show right ++ ")"

-- dsl to make expr building easier
int :: Int -> Expression
int = IntVal

bool :: Bool -> Expression
bool = BoolVal

(+.) :: Expression -> Expression -> Expression
e1 +. e2 = BinOp Plus e1 e2
pattern (:+:) :: Expression -> Expression -> Expression
pattern (:+:) e1 e2 = BinOp Plus e1 e2

(-.) :: Expression -> Expression -> Expression
e1 -. e2 = BinOp Min e1 e2
pattern (:-:) :: Expression -> Expression -> Expression
pattern (:-:) e1 e2 = BinOp Min e1 e2

(&&.) :: Expression -> Expression -> Expression
e1 &&. e2 = BinOp Conj e1 e2
pattern (:&&:) :: Expression -> Expression -> Expression
pattern (:&&:) e1 e2 = BinOp Conj e1 e2

(||.) :: Expression -> Expression -> Expression
e1 ||. e2 = BinOp Disj e1 e2
pattern (:||:) :: Expression -> Expression -> Expression
pattern (:||:) e1 e2 = BinOp Disj e1 e2

(==>.) :: Expression -> Expression -> Expression
e1 ==>. e2 = BinOp Impl e1 e2
pattern (:==>:) :: Expression -> Expression -> Expression
pattern (:==>:) e1 e2 = BinOp Impl e1 e2

(<.) :: Expression -> Expression -> Expression
e1 <. e2 = BinOp Le e1 e2
pattern (:<:) :: Expression -> Expression -> Expression
pattern (:<:) e1 e2 = BinOp Le e1 e2

(<=.) :: Expression -> Expression -> Expression
e1 <=. e2 = BinOp Leq e1 e2
pattern (:<=:) :: Expression -> Expression -> Expression
pattern (:<=:) e1 e2 = BinOp Leq e1 e2

(=.) :: Expression -> Expression -> Expression
e1 =. e2 = BinOp Eq e1 e2
pattern (:=:) :: Expression -> Expression -> Expression
pattern (:=:) e1 e2 = BinOp Eq e1 e2

data Type
  = Prim PrimitiveType
  | ArrayT Array
  deriving (Eq, Data, Typeable, Ord, Generic)


instance Show Type where
  show (Prim primType) = show primType
  show (ArrayT array) = show array

data PrimitiveType
  = Int
  | Bool
  deriving (Show, Eq, Data, Typeable, Ord, Generic)


data Array =
  Array PrimitiveType
  deriving (Eq, Data, Typeable, Ord, Generic)


instance Show Array where
  show (Array prim) = "[" ++ show prim ++ "]"
