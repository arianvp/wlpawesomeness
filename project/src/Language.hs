module Language where

import Text.PrettyPrint

data Program =
  Program [Parameter] -- ^ input parameters
          [Parameter] -- ^ output parameters
          [Statement] -- ^ body
  deriving (Eq)

instance Show Program where

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
  deriving (Eq)

instance Show Statement where
  show x = render (stmt x)

showStmts :: [Statement] -> String
showStmts s = render (stmts s)
--
var :: Variable -> Doc
var (Variable name typs) = text name

stmts :: [Statement] -> Doc
stmts ss = vcat $ punctuate (text ";") (map stmt ss)
stmt :: Statement -> Doc
stmt Skip = text "skip"
stmt (Assert expr) = text "assert" <> text (show expr)
stmt (Assume expr) = text "assume" <> text (show expr)
stmt (a := b) = text (a) <> text " := " <> text (show b)
stmt (Var vars s) =
  text "var(" <> (hcat $ punctuate (text ",") (map var vars)) <> text "){"
  $$ nest 4 (stmts s)


type Name = String

type Parameter = Variable

data Variable =
  Variable Name
           Type
  deriving (Eq)

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
  deriving (Eq, Ord, Enum)

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
data Expression
  = IntVal Int
  | BoolVal Bool
  | Name String
  | BinOp BinOp Expression Expression
  | Forall Variable
           Expression
  | Not Expression
  | ArrayAt Name
            Expression
  deriving (Eq)

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
e1 +. e2 =  BinOp Plus e1 e2

(-.) :: Expression -> Expression -> Expression
e1 -. e2 =  BinOp Min e1 e2

(&&.) :: Expression -> Expression -> Expression
e1 &&. e2 =  BinOp Conj e1 e2

(||.) :: Expression -> Expression -> Expression
e1 ||. e2 =  BinOp Disj e1 e2

(==>) :: Expression -> Expression -> Expression
e1 ==> e2 =  BinOp Impl e1 e2

(<.) :: Expression -> Expression -> Expression
e1 <. e2 =  BinOp Le e1 e2

(<=.) :: Expression -> Expression -> Expression
e1 <=. e2 =  BinOp Leq e1 e2

(=.) :: Expression -> Expression -> Expression
e1 =. e2 =  BinOp Eq e1 e2

data Type
  = Prim PrimitiveType
  | ArrayT Array
  deriving (Eq)

instance Show Type where
  show (Prim primType) = show primType
  show (ArrayT array) = show array


data PrimitiveType
  = Int
  | Bool
  deriving (Show, Eq)

data Array =
  Array PrimitiveType
  deriving (Eq)

instance Show Array where
  show (Array prim) = "[" ++ show prim ++ "]"


