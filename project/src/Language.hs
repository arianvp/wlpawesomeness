module Language where

data Program =
  Program [Parameter] -- ^ input parameters
          [Parameter] -- ^ output parameters
          [Statement] -- ^ body

data Statement
  = Skip
  | Assert Expression
  | Assume Expression
  | (:=) Name
         Expression
  | If Expression
       Statement
       Statement
  | While Expression
          [Statement]
  | Var [Variable]
        [Statement]

type Name = String

type Parameter = Variable

data Variable =
  Variable Name
           Type

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

data Type
  = Prim PrimitiveType
  | ArrayT Array

data PrimitiveType
  = Int
  | Bool

data Array =
  Array [PrimitiveType]
