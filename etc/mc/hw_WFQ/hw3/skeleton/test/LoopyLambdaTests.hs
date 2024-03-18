-- |
module LoopyLambdaTests (lambdaTests) where

import A3.LoopyLambda

import Test.HUnit

-- -- | Helper for testing α-equivalence of expressions.
-- assertAlphaEqual :: String -> Expr -> Expr -> Assertion
-- assertAlphaEqual msg e1 e2 = assertBool msg (alphaEq e1 e2)

-- | Helper for testing α-equivalence of expressions within a Maybe context.
assertAlphaEqual :: String -> Maybe Expr -> Maybe Expr -> Assertion
assertAlphaEqual msg (Just expected) (Just actual) = assertBool msg (alphaEq expected actual)
assertAlphaEqual msg Nothing Nothing = return ()  -- If both are Nothing, then they are considered equal.
assertAlphaEqual msg _ _ = assertBool msg False   -- If one is Just and the other is Nothing, they are not equal.

-- Test cases
lambdaTests :: Test
lambdaTests = TestList [
                       test1, test2, test3, test4, test5,
                       test6, test7, test8, test9, test10,
                       test11, test12, test13, test14, test15,
                       test16, test17, test18, test19, test20,
                       test21, test22, test23, test24, test25,
                       test26, test28
                       ]

-- 1. Verify that a loop with a zero counter simplifies to its second expression.
test1 :: Test
test1 = TestCase $ assertAlphaEqual "1.loop 0 e2 e3 should reduce to e2"
                               (Just (Var "x"))
                               (stepLoop (Loop Zero (Var "x") (Var "y")))

-- 2. Check that a loop with a counter incremented by one simplifies to the third expression applied to the loop with the original counter.
test2 :: Test
test2 = TestCase $ assertAlphaEqual "2.loop (1 + e1) e2 e3 should reduce to e3 (loop e1 e2 e3)"
                               (Just (App (Var "z") (Loop (Var "n") (Var "x") (Var "z"))))
                               (stepLoop (Loop (PlusOne (Var "n")) (Var "x") (Var "z")))

-- 3. Confirm that a loop with a variable counter that can't simplify remains unchanged.
test3 :: Test
test3 = TestCase $ assertAlphaEqual "3.loop e1 e2 e3 should not reduce if e1 cannot reduce"
                               (Nothing)
                               (stepLoop (Loop (Var "n") (Var "x") (Var "z")))

-- 4. Ensure that a loop with a counter that can simplify does so correctly.
test4 :: Test
test4 = TestCase $ assertAlphaEqual "4.loop e1 e2 e3 should reduce if e1 can reduce"
                               (Just (App (Var "z") (Loop Zero (Var "x") (Var "z"))))
                               (stepLoop (Loop (PlusOne Zero) (Var "x") (Var "z")))

-- 5. Make sure that the second expression in a loop isn't simplified directly.
test5 :: Test
test5 = TestCase $ assertAlphaEqual "5.loop e1 e2 e3 should not directly reduce e2"
                               (Nothing)
                               (stepLoop (Loop (Var "n") (PlusOne (Var "x")) (Var "z")))

-- 6. Test that the third expression in a loop isn't simplified directly.
test6 :: Test
test6 = TestCase $ assertAlphaEqual "6.loop e1 e2 e3 should not directly reduce e3"
                               (Nothing)
                               (stepLoop (Loop (Var "n") (Var "x") (PlusOne (Var "z"))))

-- 7. Check that nested loops simplify correctly starting from the innermost loop.
test7 :: Test
test7 = TestCase $ assertAlphaEqual "7.nested loops should reduce correctly"
                               (Just (App (Var "z") (Loop (PlusOne Zero) (Var "x") (Var "z"))))
                               (stepLoop (Loop (PlusOne (PlusOne Zero)) (Var "x") (Var "z")))

-- 8. Confirm that standalone variables remain unchanged as they can't simplify.
test8 :: Test
test8 = TestCase $ assertAlphaEqual "8.variable should not reduce"
                              Nothing
                              (stepLoop (Var "x"))

-- 9. Ensure that loops with variables as their counter don't simplify.
test9 :: Test
test9 = TestCase $ assertAlphaEqual "9.loop with variable as argument should not reduce"
                               Nothing
                               (stepLoop (Loop (Var "n") (Var "x") (Var "y")))

-- 10. Test that multiple increments on a loop's counter simplify correctly.
test10 :: Test
test10 = TestCase $ assertAlphaEqual "10.Multiple PlusOne should reduce correctly"
                               (Just (App (Var "z") (Loop (PlusOne Zero) (Var "x") (Var "z"))))
                               (stepLoop (Loop (PlusOne (PlusOne Zero)) (Var "x") (Var "z")))

-- 11. Confirm that increments outside of loops don't simplify until the loop does.
test11 :: Test
test11 = TestCase $ assertAlphaEqual "11.Reduction within PlusOne not proceed until Loop reduces"
                                (Just (PlusOne (Var "x")))
                                (stepLoop (PlusOne (Loop Zero (Var "x") (Var "y"))))

-- 12. Verify that applying a lambda function to an argument performs the expected substitution.
test12 :: Test
test12 = TestCase $ assertAlphaEqual "12.applying a lambda should perform substitution"
                              (Just (Var "y"))
                              (stepLoop (App (Lam "x" (Var "x")) (Var "y")))

-- 13. Confirm that an application of non-simplifiable expressions doesn't simplify.
test13 :: Test
test13 = TestCase $ assertAlphaEqual "13.application of non-reducible expressions should not reduce"
                              Nothing
                              (stepLoop (App (Var "x") (Var "y")))

-- 14. Check that standalone constants like zero don't simplify further.
test14 :: Test
test14 = TestCase $ assertAlphaEqual "14.Zero should not reduce"
                              Nothing
                              (stepLoop Zero)

-- 15. Ensure that an increment with a non-simplifiable argument remains unchanged.
test15 :: Test
test15 = TestCase $ assertAlphaEqual "15.PlusOne with non-reducible argument should not reduce"
                                Nothing
                                (stepLoop (PlusOne (Var "nonReducible")))

-- 16. Verify that an increment applied to zero doesn't simplify as zero can't be incremented.
test16 :: Test
test16 = TestCase $ assertAlphaEqual "16.PlusOne with a non-reducible expression should not reduce"
                              Nothing
                              (stepLoop (PlusOne Zero))

-- 17. Test that a loop with an increment that can simplify does so correctly.
test17 :: Test
test17 = TestCase $ assertAlphaEqual "17.Loop with nested PlusOne that can reduce should do so"
                                (Just (App (Var "z") (Loop Zero (Var "x") (Var "z"))))
                                (stepLoop (Loop (PlusOne Zero) (Var "x") (Var "z")))

-- 18. Confirm that an application of two non-simplifiable expressions doesn't simplify.
test18 :: Test
test18 = TestCase $ assertAlphaEqual "18.Application of non-reducible expressions should not reduce"
                                Nothing
                                (stepLoop (App (Var "nonReducible1") (Var "nonReducible2")))

-- 19. Ensure that a lambda function simplifies correctly on its own.
test19 :: Test
test19 = TestCase $ assertAlphaEqual "19.Lambda itself should reduce correctly"
                                (Just (subst "x" (Var "e2") (Var "e1")))
                                (stepLoop (App (Lam "x" (Var "e1")) (Var "e2")))

-- 20. Confirm that an application where the second expression can't simplify remains unchanged.
test20 :: Test
test20 = TestCase $ assertAlphaEqual "20.application where e2 should not reduce"
                              Nothing
                              (stepLoop (App (Var "x") (PlusOne Zero)))

-- 21. Check that a lambda function combined with a simplifiable expression simplifies correctly.
test21 :: Test
test21 = TestCase $ assertAlphaEqual "21.Lambda combine with Zero should reduce correctly"
                                (Just Zero)
                                (stepLoop (App (Lam "x" (Zero)) (Var "e2")))

-- 22. Ensure that an outer loop simplifies before an inner loop.
test22 :: Test
test22 = TestCase $ assertAlphaEqual "22.Inner Loop should reduce later than outer Loop reducing"
                                (Just (App (Var "z") (Loop Zero (Loop Zero (Var "x") (Var "y")) (Var "z"))))
                                (stepLoop (Loop (PlusOne Zero) (Loop Zero (Var "x") (Var "y")) (Var "z")))

-- 23. Verify that when the function part of an application is a simplifiable loop, it simplifies correctly.
test23 :: Test
test23 = TestCase $ assertAlphaEqual "23.Application where function part is a reducible Loop should reduce also reduce later than outer Loop"
                                (Just (App (App (Var "y") (Loop Zero (Var "x") (Var "y"))) (Var "a")))
                                (stepLoop (App (Loop (PlusOne Zero) (Var "x") (Var "y")) (Var "a")))

-- 24. Confirm that an application where the argument is a simplifiable loop doesn't simplify if the argument can't simplify.
test24 :: Test
test24 = TestCase $ assertAlphaEqual "24.Application where argument part is a reducible Loop cannot reduce since e2 cannot be reduced"
                                Nothing
                                (stepLoop (App (Var "f") (Loop (PlusOne Zero) (Var "x") (Var "y"))))

-- 25. Test that a loop with an increment in its step part simplifies correctly.
test25 :: Test
test25 = TestCase $ assertAlphaEqual "25.Reduction of a Loop with a PlusOne in the step part should reduce correctly"
                                (Just (App (Var "y") (Loop Zero (PlusOne (Var "x")) (Var "y"))))
                                (stepLoop (Loop (PlusOne Zero) (PlusOne (Var "x")) (Var "y")))


--26. construct '39' using 39 PlusOne apply to Zero
test26 :: Test
test26 = TestCase $ assertAlphaEqual "26.Constructing 39 using Loop reduction involved 39 PlusOne applied to Zero"
                               (Just (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne Zero))))))))))))))))))))))))))))))))))))))))
                               (stepLoop (Loop Zero (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne Zero))))))))))))))))))))))))))))))))))))))) (Var "nomatter")))
-- Repeat this 39 times

--27. construct '39' using recursion involving Plusone and Zero
test27 :: Expr
test27 = constructNumber 39
  where                                                     -- Constructs a recursion expression that represents the number 39.
    constructNumber 0 = Zero
    constructNumber n = PlusOne (constructNumber (n - 1))

--28. construct '39' using only Var
test28 :: Test
test28 = TestCase $ assertAlphaEqual "28.Constructing 39 using Loop reduction"
                               (Just (Var "39"))
                               (stepLoop (Loop Zero (Var "39") (Var "y")))