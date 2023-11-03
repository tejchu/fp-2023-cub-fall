module TypeCheck where

import Syntax
import qualified Data.Map as M
import Control.Monad (guard)

type Env = M.Map String Type

typeCheckEmpty :: Term String -> Either String Type
typeCheckEmpty = typeCheck M.empty

typeCheck :: Env -> Term String -> Either String Type
typeCheck env (Var v) =
  case M.lookup v env of
    Just t -> Right t
    Nothing -> Left $ "Variable '" ++ v ++ "' not in environment"
typeCheck env (Abs x t b) = do
  let env' = M.insert x t env
  t1 <- typeCheck env' b
  Right (Arrow t t1)
typeCheck env (App m n) = do
  t1 <- typeCheck env m
  t2 <- typeCheck env n
  case t1 of
    Arrow t11 t12 | t2 == t11 -> Right t12
    _ -> Left "Type mismatch in application"
typeCheck _ (BoolLit _) =
  Right Bool
typeCheck env (If c t e) = do
  ct <- typeCheck env c
  case ct of
    Bool -> do
      tt <- typeCheck env t
      et <- typeCheck env e
      if tt == et
        then Right tt
        else Left "Branches in 'if' statement have different types"
    _ -> Left "Condition in 'if' statement is not of type Bool"

