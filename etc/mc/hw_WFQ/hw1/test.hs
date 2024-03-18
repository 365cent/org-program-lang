-- | Tests for Assignment 1.
module Main where

import A1_400298939
import Test.HUnit
import System.Exit

tests :: Test
tests = TestList
  [ "EInt test" ~: evalExpr (EInt 5) ~?= VInt 5  --test if EInt output regular num
  , "ECh test" ~: evalExpr (ECh 'a') ~?= VString "a" --test if ECh output regular one-charator String
  , "EBool test" ~: evalExpr (EBool True) ~?= VBool True --test if EBool output regular Bool value
  , "EString test" ~: evalExpr (EString "hello") ~?= VString "hello" --test if EString output regular String

  , "EAdd test" ~: evalExpr (EAdd (EInt 3) (EInt 4)) ~?= VInt 7 --test if EAdd can handle one regular sum
  , "EAdd test2" ~: evalExpr (EAdd (EInt 5) $ EAdd (EInt 3) (EInt 4)) ~?= VInt 12 --test if EAdd can handle multiple regular sum
  , "EAdd test3" ~: evalExpr (EAdd (EString "3") (EInt 4)) ~?= VError --test if EAdd output error when type is wrong
  , "EAdd test4" ~: evalExpr (EAdd (EInt (10^30)) (EInt (10^30))) ~?= VInt (2 * 10^30) --test if EAdd can handle large number sum
  , "EAdd test5" ~: evalExpr (EAdd (EInt 3) $ ENeg (EInt 4)) ~?= VInt (-1) --test if EAdd can handle negative number sum

  , "EMul test" ~: evalExpr (EMul (EInt 3) (EInt 4)) ~?= VInt 12 --test if EMul can handle one regular multiplication
  , "EMul test2" ~: evalExpr (EMul (EInt 2) $ EMul (EInt 3) (EInt 4)) ~?= VInt 24 --test if EMul can handle multiple regular multiplication
  , "EMul test3" ~: evalExpr (EMul (EString "3") (EInt 4)) ~?= VError --test if EMul output error when type is wrong
  , "EMul test4" ~: evalExpr (EMul (EInt (10^10)) (EInt (10^10))) ~?= VInt (10^20) --test if EAdd can handle large number multiplication
  , "EMul test5" ~: evalExpr (EMul (EInt 3) $ ENeg (EInt 4)) ~?= VInt (-12) --test if EAdd can handle negative number multiplication

  , "ENeg test" ~: evalExpr (ENeg (EInt 3)) ~?= VInt (-3) --test if ENeg can handle one regular negation
  , "ENeg test2" ~: evalExpr (ENeg $ (ENeg (EInt 3))) ~?= VInt (3) --test if ENeg can handle multiple regular negation
  , "ENeg test3" ~: evalExpr (ENeg (EInt (3^30))) ~?= VInt (-3^30) --test if ENeg can handle large number negation
  , "ENeg test4" ~: evalExpr (ENeg (EInt 0)) ~?= VInt (0) --test if ENeg can handle 0 negation
  
  , "ECat test" ~: evalExpr (ECat (EString "Hi") (EString " Jack")) ~?= VString "Hi Jack" --test if ECat can handle one regular concatenation
  , "ECat test2" ~: evalExpr (ECat (EString ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")) 
    (EString $ "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"))
    ~?= VString ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb") 
    --test if ECat can handle long string concatenation
  , "ECat test3" ~: evalExpr (ECat (EString "") (EString "")) ~?= VString "" --test if ECat can handle empty concatenation
  , "ECat test4" ~: evalExpr (ECat (EString "") (EInt 0)) ~?= VError --test if ECat can output error when type is wrong
  , "ECat test5" ~: evalExpr (ECat (EString "yes") $ ECat (EString "yes") (EString "yes")) ~?= VString "yesyesyes" --test if ECat can handle multiple concatenation  

  , "ECons test" ~: evalExpr (ECons 'H' (EString "ello")) ~?= VString "Hello" --test if ECons can handle one regular concatenation of Char and String
  , "ECons test2" ~: evalExpr (ECons ' ' (EString "hello")) ~?= VString " hello" --test if ECons can handle one regular concatenation of blank Char and String
  , "ECons test3" ~: evalExpr (ECons 'c' (EString "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb")) 
    ~?= VString "caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" 
    --test if ECons can handle one regular concatenation of Char and long String
  , "ECons test4" ~: evalExpr (ECons 'H' (EInt 0)) ~?= VError --test if ECons can output error when type is wrong
  , "ECons test5" ~: evalExpr (ECons 'H' $ ECons 'i' (EString " jack")) ~?= VString "Hi jack" --test if ECons can handle multiple concatenation of Char and String

  , "EAnd test" ~: evalExpr (EAnd (EBool True) (EBool False)) ~?= VBool False --test if EAnd can handle one regular And of Bool giving False
  , "EAnd test2" ~: evalExpr (EAnd (EBool True) (EBool True)) ~?= VBool True --test if EAnd can handle one regular And of Bool giving True
  , "EAnd test3" ~: evalExpr (EAnd (EBool True) (EInt 0)) ~?= VError --test if EAnd output error when type is wrong
  , "EAnd test4" ~: evalExpr (EAnd (EBool False) $ EAnd (EBool True) (EBool True)) ~?= VBool False --test if EAnd can handle multiple And of Bool
  
  , "EXor test" ~: evalExpr (EXor (EBool True) (EBool False)) ~?= VBool True --test if EXor can handle one regular Xor of Bool giving True
  , "EXor test2" ~: evalExpr (EXor (EBool True) (EBool True)) ~?= VBool False --test if EXor can handle one regular Xor of Bool giving False
  , "EXor test3" ~: evalExpr (EXor (EBool True) (EInt 0)) ~?= VError --test if EAnd output error when type is wrong
  , "EXor test4" ~: evalExpr (EXor (EBool True) $ EXor (EBool True) (EBool False)) ~?= VBool False --test if EAnd can handle multiple Xor of Bool

  , "EIf test" ~: evalExpr (EIf (EBool True) (EInt 3) (EInt 4)) ~?= VInt 3 --test if EIf can handle one regular If condition when is True
  , "EIf test2" ~: evalExpr (EIf (EBool False) (EInt 3) (EInt 4)) ~?= VInt 4 --test if EIf can handle one regular If condition when is False
  , "EIf test3" ~: evalExpr (EIf (EInt 0) (EInt 3) (EInt 4)) ~?= VError --test if EIf ouput error when type is wrong
  , "EIf test4" ~: evalExpr (EIf (EIf (EBool True) (EBool True) (EInt 4)) (EString("first is right")) (EString("second is right")))
    ~?= VString "first is right" --test if EIf can handle multiple If condition
  
  , "EShowInt test" ~: evalExpr (EShowInt (EInt 3)) ~?= VString "3" --test if EShowInt can handle one regular transformation of Int to String
  , "EShowInt test2" ~: evalExpr (EShowInt (EInt (10^20))) ~?= VString (show (10^20)) --test if EShowInt can handle large number transformation to String
  , "EShowInt test3" ~: evalExpr (EShowInt (ENeg (EInt 3))) ~?= VString "-3" --test if EShowInt can handle transformation of negative int to String
  , "EShowInt test" ~: evalExpr (EShowInt (EAdd (EInt 3) (EInt 4))) ~?= VString "7" --test if EShowInt can handle transformation of algorithm to String

  --student Id build from individual characters
  , "ECons & ECat test1" ~: evalExpr (ECons '4' (ECat (EString "00") (EString "298939"))) ~?= VString "400298939"
  , "ECons & ECat test2" ~: evalExpr (ECat (EString "400") (ECons '2' (EString "98939"))) ~?= VString "400298939"
  , "ECons & ECat test3" ~: evalExpr (ECat (ECat (EString "400") (ECons '2' (EString "989"))) (EString "39")) ~?= VString "400298939"

  --student Id build as VInt using computation
  , "EShowInt computation test1" ~: evalExpr (EShowInt $ EAdd (EAdd (EAdd (EInt 400000000) (EInt 290000)) (EInt 8900)) (EInt 39)) ~?= VString "400298939"
  , "EShowInt computation test2" ~: evalExpr (EShowInt (EAdd (EAdd (EMul (EInt 100000000) (EInt 4)) (EMul (EInt 100000) (EInt 2))) (EInt 98939))) ~?= VString "400298939"
  , "EShowInt computation test3" ~: evalExpr (EShowInt (EAdd (EInt 400000000) (EAdd (EInt 298900) (EMul (ENeg (EInt (-13)))(EInt 3))))) ~?= VString "400298939"
  ]

-- Run the test suite. Please do not touch this!
main :: IO ()
main = do
    counts <- runTestTT tests 
    if errors counts + failures counts == 0 then
      exitSuccess
    else
      exitFailure
