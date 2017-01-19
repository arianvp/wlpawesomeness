{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances #-}

module Language
  ( Program(..)
  , Name
  , Variable(..)
  , Statement(..)
  , Expression(..)
  , PrimitiveType(..)
  , Array(..)
  , Type(..)
  , BinOp(..)
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
  , pattern (:/:)
  , pattern (:\:)
  , int
  , bool
  , showStmts
  , programToStmt
  ) where

import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.Generics.Uniplate.Data ()
import Text.PrettyPrint hiding (int)

data Program =
  Program [Parameter]
          [Parameter]
          [Statement]
  deriving (Eq, Data, Typeable)


prgrm :: Program -> Doc
prgrm (Program ins outs s) =
  text "(" <> (hcat $ punctuate (text ",") (map (text . show) ins)) <> text "|" <>
  (hcat $ punctuate (text ",") (map (text . show) outs)) <>
  text "){" $$
  nest 2 (stmts s) $$
  text "}"

instance Show Program where
  show = render . prgrm


programToStmt :: Program -> [Statement]
programToStmt (Program ins outs s) =
  [Var (ins ++ outs) s]

data Statement
  = Skip
  | Assert Expression
  | Assume Expression
  | (:=) String
         Expression
  | If Expression
       [Statement]
       [Statement]
  | While Expression
          [Statement]
  | Var [Variable]
        [Statement]
  | ProgramCall AsgTargets Program Expressions --("r1","r2") (:.=) program1 (first_input_expression,second_inp_expr)
  deriving ( Eq, Data, Typeable)

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
stmt (a := b) = text a <> text " := " <> text (show b)
stmt (Var vars s) =
  text "var(" <> (hcat $ punctuate (text ",") (map (text . show) vars)) <>
  text "){" $$
  nest 2 (stmts s) $$ 
  text "}"

type Name = String

type Parameter = Variable

data Variable =
  Variable Name
           Type
  deriving (Eq, Data, Typeable)

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
  | Then
  | Else
  deriving (Eq, Data, Typeable)

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
	  --Then -> "->"
	  --Else -> "|"
  -- todo fixitivity

data Expressions
	= (:\:) Expression Expressions -- instead of comma
	| Expression
	
data AsgTargets 
	= (:/:) AsgTarget AsgTargets 
	| AsgTarget
	
data AsgTarget
	= Targ String
	| Targ1 Name Expression
  
data Expression
  = IntVal Int
  | BoolVal Bool
  | Name String
  | BinOp BinOp
          Expression
          Expression
  | Forall Variable
           Expression
  | Not Expression
  | ArrayAt Name
            Expression
  deriving (Eq, Data, Typeable)

instance Show Expression where
  show x =
    case x of
      IntVal i -> show i
      BoolVal b -> show b
      Name s -> s
      BinOp binOp e1 e2 -> "(" ++ show e1 ++ show binOp ++ show e2 ++ ")"
      Forall var e -> "∀" ++ show var ++ "." ++ show e
      Not e -> "¬" ++ show e
      ArrayAt n e -> n ++ "[" ++ show e ++ "]"

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
  deriving (Eq, Data, Typeable)

instance Show Type where
  show (Prim primType) = show primType
  show (ArrayT array) = show array

data PrimitiveType
  = Int
  | Bool
  deriving (Show, Eq, Data, Typeable)

data Array =
  Array PrimitiveType
  deriving (Eq, Data, Typeable)

instance Show Array where
  show (Array prim) = "[" ++ show prim ++ "]"
