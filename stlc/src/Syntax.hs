
{-# LANGUAGE FlexibleInstances #-}

module Syntax where

import Text.Printf (printf)
import Data.Char (chr)
import Data.List (nub)
import Data.Maybe (fromJust)
import qualified Data.Set as S

data Type
  = TyVar String
  | Arrow Type Type
  | Bool
  | Int -- add int type, I hope the name is fine
  deriving (Eq, Ord)

instance Show Type where
  show (TyVar v) = v
  show (Arrow (Arrow t1 t2) t3) = printf  "(%s -> %s) -> %s" (show t1) (show t2) (show t3)
  show (Arrow t1 t2) = printf  "%s -> %s" (show t1) (show t2)
  show (Bool) = "Bool"
  show (Int) = "Int" -- add show for ints

data Term a
  = Var a
  | Abs a Type (Term a)
  | App (Term a) (Term a)
  | BoolLit Bool
  | If (Term a) (Term a) (Term a)
  | IntLit Int -- add int literal
  | Add (Term a) (Term a) -- add arithmetic ops
  | Sub (Term a) (Term a)
  | Mul (Term a) (Term a)
  | Div (Term a) (Term a)
  | Let a (Term a) (Term a) -- add let binding
  deriving (Eq)

instance Show (Term String) where
  show (Var x) = x
  show (Abs x typ t) = printf "λ%s:%s.%s" x (show typ) (show t)
  show (App t1 t2) = printf "(%s) (%s)" (show t1) (show t2)
  show (BoolLit b) = printf "%s" (show b)
  show (If c t e) = printf "if %s then %s else %s" (show c) (show t) (show e)
  show (IntLit i) = printf "%s" (show i) -- add show for ints
  show (Add t1 t2) = printf "(%s) + (%s)" (show t1) (show t2) -- add show for arithmetic ops
  show (Sub t1 t2) = printf "(%s) - (%s)" (show t1) (show t2)
  show (Mul t1 t2) = printf "(%s) * (%s)" (show t1) (show t2)
  show (Div t1 t2) = printf "(%s) / (%s)" (show t1) (show t2)

freeVars :: Ord a => Term a -> S.Set a
freeVars (Var a) = S.singleton a
freeVars (Abs a _ t) = S.delete a (freeVars t)
freeVars (App t1 t2) = S.union (freeVars t1) (freeVars t2)
freeVars (BoolLit _) = S.empty
freeVars (If c t e) = S.unions (freeVars <$> [c, t, e])
freeVars (IntLit _) = S.empty -- add free vars for ints
freeVars (Add t1 t2) = S.union (freeVars t1) (freeVars t2) -- I am not sure if I need these and whether I implemented them correctly
freeVars (Sub t1 t2) = S.union (freeVars t1) (freeVars t2)
freeVars (Mul t1 t2) = S.union (freeVars t1) (freeVars t2)
freeVars (Div t1 t2) = S.union (freeVars t1) (freeVars t2)
freeVars (Let x t1 t2) = S.union (freeVars t1) (S.delete x (freeVars t2)) -- for let binding
