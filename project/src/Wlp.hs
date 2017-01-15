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
    -- we _KNOW_ n if fresh due to preprocessing ,so
    -- this is safe
    Var n s ->  foldr Forall (wlp s postc) n
      -- foldr Forall (calcWlp s (calcWlp stmts postc)) n
    -- TODO: arrays
    (n := e) -> substitute (Name n) e (wlp stmts postc)
    While _ _ -> error "wlp only supports cannonical statements, no branching"
    If _ _ _ -> error "wlp only supports cannonical statements, no branching"
