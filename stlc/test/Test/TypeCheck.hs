module Test.TypeCheck where

import Test.Tasty.HUnit (Assertion, (@?=))

import Syntax
import TypeCheck
import qualified Data.Map as M

unit_typecheck = do
    check (Abs "x" Bool (Var "x")) (Right $ Arrow Bool Bool)
    check (If (App (Abs "x" Bool (Var "x")) (BoolLit True)) (BoolLit True) (BoolLit False)) (Right Bool)
    check (Abs "f" (Arrow (TyVar "b") (TyVar "c")) (Abs "g" (Arrow (TyVar "a") (TyVar "b")) (Abs "x" (TyVar "a") (App (Var "f") (App (Var "g") (Var "x")))))) (Right $ Arrow (Arrow (TyVar "b") (TyVar "c")) (Arrow (Arrow (TyVar "a") (TyVar "b")) (Arrow (TyVar "a") (TyVar "c"))))
    -- Test let expression
    check (Let "x" (BoolLit True) (Var "x")) (Right Bool)
  where
    check term typ =
      typeCheckEmpty term @?= typ

unit_typecheck_env = do
    checkEnv (M.singleton "f" (Arrow Bool Bool)) (Abs "x" Bool (App (Var "f") (If (Var "x") (BoolLit False) (Var "x")))) (Right $ Arrow Bool Bool)
    -- Test let expression in env
    checkEnv M.empty (Let "x" (BoolLit True) (Var "x")) (Right Bool)
  where
    checkEnv env term typ = typeCheck env term @?= typ
