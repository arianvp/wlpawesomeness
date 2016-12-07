module Language where

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
  deriving (Show,Eq)

type Unique = Int

type Name = String

type Parameter = Variable

data Variable =
  Variable Name
           Type
  deriving (Show, Eq)

data Expression
  = IntVal Int
  | BoolVal Bool
  | Name Name
  | (:+:) Expression
          Expression
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
  deriving (Show,Eq)

data Type
  = Prim PrimitiveType
  | ArrayT Array
  deriving (Show, Eq)

data PrimitiveType
  = Int
  | Bool
  deriving (Show, Eq)

data Array =
  Array [PrimitiveType]
  deriving (Show, Eq)
