module Main(main) where

import Test.HUnit
import A5
import Data.Maybe (fromJust)
import Data.Either (isLeft)

-- Looked up on https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Maybe.html
-- https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Either.html

-- Helper function to compare two types for equality
typesEqual :: Type -> Type -> Bool
typesEqual (TpVar a) (TpVar b) = a == b
typesEqual (FnTp t1 t2) (FnTp t3 t4) = typesEqual t1 t3 && typesEqual t2 t4
typesEqual UnitTp UnitTp = True
typesEqual (PairTp t1 t2) (PairTp t3 t4) = typesEqual t1 t3 && typesEqual t2 t4
typesEqual BoolTp BoolTp = True
typesEqual NatTp NatTp = True
typesEqual _ _ = False


-- Helper function to extract type from the infer function
extractInferredType :: Either String (TypedExpr, Type) -> Maybe Type
extractInferredType (Right (_, t)) = Just t
extractInferredType _ = Nothing

-- TODO: Write tests for A5 typecheckor
testA5 :: Test
testA5 = TestList [
                    testBoolTrue, testBoolFalse, testLambda, testLet, testPair,
                    testIfThenElse, testZero, testSuccZero, testPairNumbers,
                    testNegation, testInvalidBool, testNestedSucc,
                    testInvalidNum, testNestedPairs, testInvalidPair,
                    testHigherOrderFunc, testInvalidFunc,testUnboundVContext,
                    testPairFunc, testMixedTypes, testRecursive, testComplexLetBind
                  ]
-- Test Boolean True
testBoolTrue :: Test
testBoolTrue = TestCase $
    let expectedType = BoolTp
        actualTypeResult = infer [] TT
    in case actualTypeResult of
         Right (_, actualType) -> assertBool "Types should be equal" (typesEqual expectedType actualType)
         Left err -> assertFailure $ "Type inference failed with error: " ++ err

-- Test Boolean False
testBoolFalse :: Test
testBoolFalse = TestCase $
    let expectedType = BoolTp
        actualTypeResult = infer [] FF
    in case actualTypeResult of
         Right (_, actualType) -> assertBool "Types should be equal" (typesEqual expectedType actualType)
         Left err -> assertFailure $ "Type inference failed with error: " ++ err


-- Test an identity lambda function
testLambda :: Test
testLambda = TestCase $ 
    let expectedType = FnTp (TpVar "a") (TpVar "a")
        actualTypeResult = infer [] (Lam "x" (Var "x"))
    in case actualTypeResult of
         Right (_, actualType) -> assertBool "Types should be equal" (typesEqual expectedType actualType)
         Left err -> assertFailure $ "Type inference failed with error: " ++ err

-- Test a simple let binding
testLet :: Test
testLet = TestCase $
    let expectedType = FnTp (TpVar "b") (TpVar "b")
        actualTypeResult = infer [] (Let "id" (Lam "x" (Var "x")) (Var "id"))
    in case actualTypeResult of
         Right (_, actualType) -> assertBool "Types should be equal" (typesEqual expectedType actualType)
         Left err -> assertFailure $ "Type inference failed with error: " ++ err

-- Test a pair construction
testPair :: Test
testPair = TestCase $
    let expectedType = PairTp NatTp BoolTp
        actualTypeResult = infer [] (Pair (Zero) TT)
    in case actualTypeResult of
         Right (_, actualType) -> assertBool "Types should be equal" (typesEqual expectedType actualType)
         Left err -> assertFailure $ "Type inference failed with error: " ++ err

-- Test If-Then-Else expression
testIfThenElse :: Test
testIfThenElse = TestCase $
    let expectedType = BoolTp
        actualTypeResult = infer [] (If TT FF TT)
    in case actualTypeResult of
         Right (_, actualType) -> assertBool "Types should be equal" (typesEqual expectedType actualType)
         Left err -> assertFailure $ "Type inference failed with error: " ++ err

-- Test zero
testZero :: Test
testZero = TestCase $
    let expectedType = NatTp
        actualTypeResult = infer [] Zero
    in case actualTypeResult of
         Right (_, actualType) -> assertBool "Types should be equal" (typesEqual expectedType actualType)
         Left err -> assertFailure $ "Type inference failed with error: " ++ err

-- Test Successor of zero
testSuccZero :: Test
testSuccZero = TestCase $
    let expectedType = NatTp
        actualTypeResult = infer [] (Succ Zero)
    in case actualTypeResult of
         Right (_, actualType) -> assertBool "Types should be equal" (typesEqual expectedType actualType)
         Left err -> assertFailure $ "Type inference failed with error: " ++ err


-- Test Pair of numbers
testPairNumbers :: Test
testPairNumbers = TestCase $
    let expectedType = PairTp NatTp NatTp
        actualTypeResult = infer [] (Pair (Succ Zero) Zero)
    in case actualTypeResult of
         Right (_, actualType) -> assertBool "Types should be equal" (typesEqual expectedType actualType)
         Left err -> assertFailure $ "Type inference failed with error: " ++ err


-- Test logical negation
testNegation :: Test
testNegation = TestCase $
    let expectedType = BoolTp
        actualTypeResult = infer [] (If TT FF TT)
    in case actualTypeResult of
         Right (_, actualType) -> assertBool "Type of logical negation should be BoolTp" (typesEqual expectedType actualType)
         Left err -> assertFailure $ "Type inference failed with error: " ++ err

-- Test invalid boolean usage
testInvalidBool :: Test
testInvalidBool = TestCase $
    assertBool "Detect type error in boolean expression" $
    isLeft (infer [] (If Zero TT FF))


-- Test nested successor expressions
testNestedSucc :: Test
testNestedSucc = TestCase $
    let expectedType = NatTp
        actualTypeResult = infer [] (Succ (Succ Zero))
    in case actualTypeResult of
         Right (_, actualType) -> assertBool "Type of nested Succ should be NatTp" (typesEqual expectedType actualType)
         Left err -> assertFailure $ "Type inference failed with error: " ++ err

-- Test invalid numeric usage
testInvalidNum :: Test
testInvalidNum = TestCase $
    assertBool "Detect type error in numeric expression" $
    isLeft (infer [] (Succ TT))


-- Test nested pairs
testNestedPairs :: Test
testNestedPairs = TestCase $
    let expectedType = PairTp (PairTp NatTp BoolTp) (PairTp BoolTp NatTp)
        actualTypeResult = infer [] (Pair (Pair Zero TT) (Pair FF Zero))
    in case actualTypeResult of
         Right (_, actualType) -> assertBool "Type of nested pairs should be PairTp (PairTp NatTp BoolTp) (PairTp BoolTp NatTp)" (typesEqual expectedType actualType)
         Left err -> assertFailure $ "Type inference failed with error: " ++ err

-- Test invalid pair usage
testInvalidPair :: Test
testInvalidPair = TestCase $
    assertBool "Detect type error in pair expression" $
    isLeft (infer [] (Fst TT))


-- Test higher-order functions
testHigherOrderFunc :: Test
testHigherOrderFunc = TestCase $
    let expectedType = FnTp (FnTp NatTp (TpVar "b")) (TpVar "b")
        actualTypeResult = infer [] (Lam "f" (App (Var "f") Zero))
    in case actualTypeResult of
         Right (_, actualType) -> assertBool "Type of higher-order function is FnTp (FnTp NatTp (TpVar 'b')) (TpVar 'b')" (typesEqual expectedType actualType)
         Left err -> assertFailure $ "Type inference failed with error: " ++ err

-- Test invalid function application
testInvalidFunc :: Test
testInvalidFunc = TestCase $
    assertBool "Detect type error in function application" $
    isLeft (infer [] (App (Lam "x" (Succ (Var "x"))) FF))


-- Test unbound variable in context
testUnboundVContext :: Test
testUnboundVContext = TestCase $
    assertBool "Detect unbound variable error in context" $
    isLeft (infer [("x", Mono NatTp)] (Var "y"))


-- Test nested pair containing a function
-- Test a nested pair where the first element is a lambda function (NatTp -> NatTp) 
-- and the second element is a pair of boolean values.
testPairFunc :: Test
testPairFunc = TestCase $
    let expr = Pair (Lam "x" (Succ (Var "x"))) (Pair TT FF)
        expectedType = PairTp (FnTp NatTp NatTp) (PairTp BoolTp BoolTp)
    in assertBool "Type of nested pair with function should be PairTp (FnTp NatTp NatTp) (PairTp BoolTp BoolTp)" $
       typesEqual (fromJust (extractInferredType (infer [] expr))) expectedType


-- Test conditional expression with mixed types
-- Test a conditional expression (If-Then-Else) where the branches return different types.
-- This should result in a type error as both branches of a conditional should have the same type.
testMixedTypes :: Test
testMixedTypes = TestCase $
    assertBool "Detect type error in conditional with mixed types" $
    isLeft (infer [] (If TT (Pair Zero Zero) (Succ Zero)))


-- Test recursive function 
-- Test a recursive function, resembling the structure as it involves self-application which can lead to complex recursive type scenarios.
testRecursiveFunction :: Test
testRecursive :: Test
testRecursive = TestCase $
    let expr = App (Lam "f" (App (Var "f") (Var "f"))) (Lam "x" (Var "x"))
    in assertBool "Detect type error in recursive function application" $
       isLeft (infer [] expr)


-- Test complex let bindings with nested scopes
-- where a function is defined in the let binding and then applied twice.
-- The test checks if the typechecker can handle nested scopes and multiple applications of the same function.
testComplexLetBind :: Test
testComplexLetBind = TestCase $
    let expr = Let "f" (Lam "x" (Succ (Var "x"))) (App (Var "f") (App (Var "f") Zero))
    in assertBool "Type of complex let bindings should be NatTp" $
       typesEqual (fromJust (extractInferredType (infer [] expr))) NatTp



main :: IO ()
main = do
  results <- runTestTT testA5
  if errors results + failures results == 0
    then return ()
    else error "Tests failed."
