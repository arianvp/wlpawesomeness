{-#LANGUAGE FlexibleInstances #-}
-- The canonical language has no
-- compound structures
module Language.Canonical
  ( Expression(..)
  , Variable(..)
  , Statement(..)
  , Name(..)
  , Unique(..)
  , PrimitiveType(..)
  , Type(..)
  , showStmts
  ) where

import Text.PrettyPrint
import Language
       (Expression(..), Variable(..), Name(..), Unique(..),
        PrimitiveType(..), Type(..))

data Statement
  = Skip
  | Assert Expression
  | Assume Expression
  | (:=) Name
         Expression
  | Var [Variable]
        [Statement]
  deriving (Eq, Show)

{-instance Show Statement where
  show = render . stmt-}

showStmts = putStrLn . render . stmts

stmts :: [Statement] -> Doc
stmts ss = vcat $ punctuate (text ";") (map stmt ss)
stmt :: Statement -> Doc
stmt Skip = text "skip"
stmt (Assert expr) = text "assert" <> text (show expr)
stmt (Assume expr) = text "assume" <> text (show expr)
stmt (a := b) = text (show a) <> text " := " <> text (show b)
