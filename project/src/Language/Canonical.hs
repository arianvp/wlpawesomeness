-- The canonical language has no
-- compound structures
module Language.Canonical
  ( Expression(..)
  , Variable(..)
  , Statement(..)
  , Name(..)
  , Unique(..)
  ) where

import Language
       (Expression(..), Variable(..), Name(..), Unique(..))

data Statement
  = Skip
  | Assert Expression
  | Assume Expression
  | (:=) Name
         Expression
  | Var [Variable]
        [Statement]
