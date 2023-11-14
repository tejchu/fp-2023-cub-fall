{-# LANGUAGE OverloadedStrings #-}

module Test.Parser where

import Test.Tasty.HUnit (Assertion, (@?=), assertBool, assertFailure)

import Parser
import Syntax
import Data.Either (isLeft)
import Data.Text (pack)

unit_parser_lits = do
  boolLitTests
  intLitTests

unit_parser_vars = do
  varTests

unit_parser_abs = do
  absTests
  absTestsFail

unit_parser_app = do
  appTests

unit_parser_let = do
  letTests



letTests = do
  parseLambdaTerm "Let x = True In x" @?= Right (Let "x" (BoolLit True) (Var "x"))

  
  
boolLitTests = do
      parseLambdaTerm "True" @?= Right (BoolLit True)
      parseLambdaTerm "False" @?= Right (BoolLit False)


intLitTests = do
      parseLambdaTerm "42" @?= Right (IntLit 42)
      parseLambdaTerm "876510" @?= Right (IntLit (876510))

varTests = do
      parseLambdaTerm "x" @?= Right (Var "x")
      parseLambdaTerm "foo" @?= Right (Var "foo")
      parseLambdaTerm "x42" @?= Right (Var "x42")
      


absTests = do
      parseLambdaTerm "\\x:Bool. x" @?= Right (Abs "x" Bool (Var "x"))
      parseLambdaTerm "\\x:Bool. \\y:Bool. x" @?= Right (Abs "x" Bool (Abs "y" Bool (Var "x")))

absTestsFail = do
    let result = parseLambdaTerm "\\x. x"
    case result of
        Left _ -> return () -- Expected a failure, do nothing
        _ -> assertBool "Should have failed but succeeded" False

appTests = do
        parseLambdaTerm "(\\x:Bool.x) (y)" @?= Right (App (Abs "x" Bool (Var "x")) (Var "y"))
        parseLambdaTerm "(\\x:Bool.x) (y) (z)" @?= Right (App (App (Abs "x" Bool (Var "x")) (Var "y")) (Var "z"))
        parseLambdaTerm "(\\x:Bool.x) (y) (z) (w)" @?= Right (App (App (App (Abs "x" Bool (Var "x")) (Var "y")) (Var "z")) (Var "w"))

