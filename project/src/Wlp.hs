module Wlp where

import Language
import Substitute

wlp :: [Statement] -> Expression -> Expression
wlp [] postc = postc
wlp (stmt:stmts) postc =
  case stmt of
    -- skip is skipped
    Skip -> wlp stmts postc
    -- assert is conjunction
    Assert e -> e &&. wlp stmts postc
    -- asssume is implication
    Assume e -> e ==>. wlp stmts postc
    Var n s ->
      -- var is simply introducing a forAll for each variable
      foldr forAll (wlp s (wlp stmts postc)) n
    -- assignment to a name is substituting name by e in the wlp
    (N n := e) ->
      -- TODO substitute an array by another array!!
      -- however, we should build in a hack such that we can
      -- replace array names as well
      substitute (Name n) e (wlp stmts postc)
    (A n i := e2) -> substituteArray (A n i) e2 (wlp stmts postc)
    _ -> error "wlp only supports cannonical statements, no branching"
