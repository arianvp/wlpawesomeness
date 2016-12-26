module Property where

{-import Data.Map (Map)
import Language

import Test.QuickCheck.Property
-- Generates a QuickCheck propery based on


-- in this module, we asusme expressions are normalized
-- such that they only contain one outer implicaton
--
-- there are of the form:
--
--  (c1 /\ c2 /\ c3 /\ ...  /\ cn) ==> f
--
--  where we want to falsify f by  adding
--  the constrains

-- the constraints form a generator,
-- whilst f forms a property

data AGen = IntGen (Gen Int) | BoolGen (Gen Bool)

-- each variable has an associated generator
type Gens = Map Name AGen


{-
 - The constrains can concern multiple variables
 -}
instance Testable Expression where
  property (constraints :==>: prop) =  undefined -}
