-- | Tests for question 1
module SKITests (skiTests) where

import A3.SKI
import Test.HUnit

-- TODO: Write tests for 'SKI'.
skiTests :: Test
skiTests = TestList [
                    testS, testK, testI, 
                    testLeft, testRight, 
                    testNoreduction, testsingle,
                    testpartial, testexcess,
                    testmulti
                    ]

--1 Test the S rule: Sxyz -> xz(yz)
testS :: Test
testS = TestCase $ assertEqual "(Sxyz -> xz(yz)), have (App (App (App S I)I)I)" (Just (App (App I I)(App I I))) (ski (App (App (App S I) I) I))

--2 Test the K rule: Kxy -> x
testK :: Test
testK = TestCase $ assertEqual "(Kxy -> x), have (App (App K I) S)" (Just I) (ski (App (App K I)S))

--3 Test the I rule: Ix -> x
testI :: Test
testI = TestCase $ assertEqual "(Ix -> x), have (App I S)" (Just S) (ski (App I S))

--4 Test reduction on the left side 
testLeft :: Test
testLeft = TestCase $ assertEqual "left side reduction, have (App (App (App S I) I) (App K I))"
                                    (Just (App (App I (App K I)) (App I (App K I)))) (ski (App (App (App S I)I) (App K I)))

--5 Test reduction on the right side 
testRight :: Test
testRight = TestCase $ assertEqual "right side reduction, have (App K (App (App S I)I))"
                                      (Just (App K(App(App I I) (App I I)))) (ski (App K (App (App (App S I)I) I)))

--6 Test no reduction possible
testNoreduction :: Test
testNoreduction = TestCase $ assertEqual "no reduction possible, have (App K K)" Nothing (ski (App K K))

--7 Test single S/K/I
testsingle :: Test
testsingle = TestCase $ assertEqual "single S/K/I, have I" Nothing (ski (S))

--8 Test partial apply(S only get two arguments)
testpartial :: Test
testpartial = TestCase $ assertEqual "partially applied arguments to S" Nothing (ski (App (App S I) K))

--9 Test excessive argument apply(K get three arguments)
testexcess :: Test
testexcess = TestCase $ assertEqual "excessive arguments applied to K" (Just(App I I)) (ski (App (App (App K I)I)I))

--10 Test multiple combinators need sequential reduction which shows nothing at only one step
testmulti :: Test
testmulti = TestCase $ assertEqual "multiple combinators need sequential reduction" Nothing (ski (App (App S (App K S)) (App (App S I) K)))

