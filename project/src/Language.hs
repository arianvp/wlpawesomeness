module Language where

import Text.PrettyPrint

data Program =
  Program [Parameter] -- ^ input parameters
          [Parameter] -- ^ output parameters
          [Statement] -- ^ body
  deriving (Show)

data Statement
  = Skip
  | Assert Expression
  | Assume Expression
  | (:=) Name
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
  show = render . stmt

stmts :: [Statement] -> Doc
stmts ss = vcat $ punctuate (text ";") (map stmt ss)
stmt :: Statement -> Doc
stmt Skip = text "skip"
stmt (Assert expr) = text "assert" <> text (show expr)
stmt (a := b) = text (show a) <> text " := " <> text (show b)

type Unique = Int

type Name = String

type Parameter = Variable

data Variable =
  Variable Name
           Type
  deriving (Eq)

instance Show Variable where
  show (Variable name typ) = name ++ ":" ++ show typ


data Expression
  = IntVal Int
  | BoolVal Bool
  | Name Name
  | (:+:) Expression Expression
  | (:-:) Expression
          Expression
  | (:&&:) Expression
           Expression
  | (:||:) Expression
           Expression
  | (:=>:) Expression
           Expression
  | (:<:) Expression
          Expression
  | (:<=:) Expression
           Expression
  | (:=:) Expression
          Expression
  | Forall Variable
           Expression
  | Not Expression
  | ArrayAt Name
            Expression
  deriving (Eq)

bool :: Bool -> Doc
bool True = text "true"
bool False = text "false"

expr :: Expression -> Doc
expr (IntVal i) = int i
expr (BoolVal b) = bool b
expr (Name name) = text name
expr (a :+: b) = lparen <> expr a <> text "+" <> expr b <> rparen
expr (a :-: b) = lparen <> expr a <> text "-" <> expr b <> rparen
expr (a :&&: b) = lparen <> expr a <> text "&&" <> expr b <> rparen
expr (a :||: b) = lparen <> expr a <> text "||" <> expr b <> rparen
expr (a :=>: b) = lparen <> expr a <> text "=>" <> expr b <> rparen
expr (a :<: b) = lparen <> expr a <> text "<" <> expr b <> rparen
expr (a :<=: b) = lparen <> expr a <> text "<=" <> expr b <> rparen
expr (a :=: b) = lparen <> expr a <> text "=" <> expr b <> rparen

instance Show Expression where
  show = render . expr


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


