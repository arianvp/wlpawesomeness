module Wlp where

import Language
import Substitute
import GHC.Stack

wlp :: HasCallStack => [Statement] -> Expression -> Expression
wlp [] postc = postc
wlp (stmt:stmts) postc =
  case stmt of
    Skip -> wlp stmts postc
    Assert e -> e &&. wlp stmts postc
    Assume e -> e ==>. wlp stmts postc
    Var n s ->
      foldr forAll (wlp s (wlp stmts postc)) n
    (N n := e) ->
      -- TODO substitute an array by another array!!
      substitute (Name n) e (wlp stmts postc)
    (A n i := e2) -> substituteArray (A n i) e2 (wlp stmts postc)
    While _ _ -> error "wlp only supports cannonical statements, no branching"
    If _ _ _ -> error "wlp only supports cannonical statements, no branching"
