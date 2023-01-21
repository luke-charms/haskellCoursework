module Main where

import Test.HUnit
import System.Exit
import Data.Char
import Control.Monad
import Data.List
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq
import Challenges
import System.Directory
import Test.HUnit
import qualified System.Exit as Exit


allTests = TestLabel "All 6 Challenges Tests" $ TestList [tests1, tests2, tests3, tests4, tests5, tests6]

main :: IO ()
main = do
    result <- runTestTT allTests
    if failures result > 0 then putStr "Fail" else putStr "Success"


-- test1 for coursework question 1


tests1 = TestLabel "calcBBInteractions TESTS" $ TestList [test1_1, test1_2, test1_3, test1_4, test1_5, test1_6, test1_7, test1_8]

test1_1 = TestCase $ assertEqual "calcBBInteractions with grid 8 and atoms as implied:" correctInteractions1 (calcBBInteractions 8 testAtoms1 firedRays1)
test1_2 = TestCase $ assertEqual "calcBBInteractions with grid 1 and atoms as implied:" correctInteractions2 (calcBBInteractions 1 testAtoms2 firedRays2)
test1_3 = TestCase $ assertEqual "calcBBInteractions with grid 2 and atoms as implied:" correctInteractions3 (calcBBInteractions 2 testAtoms3 firedRays3)
test1_4 = TestCase $ assertEqual "calcBBInteractions with grid 3 and atoms as implied:" correctInteractions4 (calcBBInteractions 3 testAtoms4 firedRays4)
test1_5 = TestCase $ assertEqual "calcBBInteractions with grid 4 and atoms as implied:" correctInteractions5 (calcBBInteractions 4 testAtoms5 firedRays5)
test1_6 = TestCase $ assertEqual "calcBBInteractions with grid 4 and atoms as implied:" correctInteractions6 (calcBBInteractions 4 testAtoms6 firedRays6)
test1_7 = TestCase $ assertEqual "calcBBInteractions with grid 8 and atoms as implied:" correctInteractions7 (calcBBInteractions 8 testAtoms7 firedRays7)
test1_8 = TestCase $ assertEqual "calcBBInteractions with grid 8 and atoms as implied:" correctInteractions8 (calcBBInteractions 8 testAtoms8 firedRays8)

-- test from coursework with grid size 8
correctInteractions1 :: Interactions
correctInteractions1 = [
 ((North,1),Path (West,2)),
 ((North,5),Path (East,5)),
 ((East,7),Path (East,4)),
 ((South,7),Absorb),
 ((West,3),Absorb),
 ((West,1),Path (East,1)),
 ((South,6),Reflect),
 ((East,6),Absorb),
 ((North,3),Path (North,6))]

correctInteractions2 :: Interactions
correctInteractions2 = [
 ((North,1),Absorb),
 ((East,1),Absorb)]

correctInteractions3 :: Interactions
correctInteractions3 = [
 ((North, 1),Absorb), 
 ((East, 1),Reflect),
 ((East, 2),Absorb),
 ((West, 2),Reflect),
 ((South, 1),Reflect)]

correctInteractions4 :: Interactions
correctInteractions4 = [
 ((North, 1),Absorb),
 ((East, 3),Absorb),
 ((West, 2),Reflect),
 ((North, 3),Absorb),
 ((South, 1),Absorb)]

correctInteractions5 :: Interactions
correctInteractions5 = [
 ((North,1),Reflect),
 ((North,2),Absorb),
 ((North,3),Reflect),
 ((North,4),Absorb)]

correctInteractions6 :: Interactions
correctInteractions6 = [
 ((North, 1),Absorb),
 ((East, 2),Path (North,4)),
 ((South, 3),Reflect),
 ((West, 4),Path (South,2))]

correctInteractions7 :: Interactions
correctInteractions7 = [
 ((West, 3),Path(South, 1)),
 ((South, 5),Reflect),
 ((East, 7),Path(South, 8)),
 ((North, 5),Path(East, 1)),
 ((East, 2),Absorb),
 ((West, 6),Absorb),
 ((North, 6),Path(West, 5)),
 ((South, 1),Path(West, 3)),
 ((East , 3),Path(West, 7))]

correctInteractions8 :: Interactions
correctInteractions8 = [
 ((West, 4),Path(North, 2)),
 ((North, 6),Path(East, 2)),
 ((East, 7),Absorb),
 ((South, 4),Path(South, 5)),
 ((East, 2),Path(North, 6)),
 ((South, 5),Path(South, 4)),
 ((North, 2),Path(West, 4)),
 ((South, 1),Path(North, 1)),
 ((West , 3),Absorb),
 ((East , 3),Path(North, 7))]


-- test from coursework spec with GRID SIZE 8
testAtoms1 :: Atoms
testAtoms1 = [ (2,3) , (7,3) , (4,6) , (7,8)]

-- test with GRID SIZE 1
testAtoms2 :: Atoms
testAtoms2 = [(1,1)]

-- test with GRID SIZE 2
testAtoms3 :: Atoms
testAtoms3 = [(1,1) , (2,2)]

-- test with GRID SIZE 3
testAtoms4 :: Atoms
testAtoms4 = [(1,1) , (3,1) , (1,3) , (3,3)]

-- test with GRID SIZE 4
testAtoms5 :: Atoms
testAtoms5 = [(2,1) , (4,2) , (1,3) , (3,4)]

-- test with equal descending atoms and GRID SIZE 4
testAtoms6 :: Atoms
testAtoms6 = [(1,1) , (2,2) , (3,3) , (4,4)]

-- test with random atoms and GRID SIZE 8
testAtoms7 :: Atoms
testAtoms7 = [(2,2) , (4,2) , (3,3) , (7,6) , (6,8)]

-- test2 with random atoms and GRID SIZE 8
testAtoms8 :: Atoms
testAtoms8 = [(5,3) , (6,4) , (3,5) , (3,7) , (6,7)]


firedRays1 :: [EdgePos]
firedRays1 = [ (North, 1) , (North, 5) , (East, 7) , (South, 7) , (West, 3) , (West, 1) , (South, 6) , (East, 6) , (North , 3) ]

firedRays2 :: [EdgePos]
firedRays2 = [ (North, 1) , (East, 1)]

firedRays3 :: [EdgePos]
firedRays3 = [ (North, 1) , (East, 1) , (East, 2) , (West, 2) , (South, 1)]

firedRays4 :: [EdgePos]
firedRays4 = [ (North, 1) , (East, 3) , (West, 2) , (North, 3) , (South, 1)]

firedRays5 :: [EdgePos]
firedRays5 = [ (North, 1) , (North, 2) , (North, 3) , (North, 4)]

firedRays6 :: [EdgePos]
firedRays6 = [ (North, 1) , (East, 2) , (South, 3) , (West, 4)]

firedRays7 :: [EdgePos]
firedRays7 = [ (West, 3) , (South, 5) , (East, 7) , (North, 5) , (East, 2) , (West, 6) , (North, 6) , (South, 1) , (East , 3) ]

firedRays8 :: [EdgePos]
firedRays8 = [ (West, 4) , (North, 6) , (East, 7) , (South, 4) , (East, 2) , (South, 5) , (North, 2) , (South, 1) , (West , 3) , (East , 3) ]



-- test2 for coursework question 2


tests2 = TestLabel "solveBB TESTS" $ TestList [test2_1, test2_2, test2_3_1, test2_3_2, test2_3_3, test2_4, test2_5, test2_6]

test2_1 = TestCase $ assertEqual "solveBB with grid 8 and interactions as implied:" correctAtoms1 (solveBB 8 testInteractions1)
test2_2 = TestCase $ assertEqual "solveBB with grid 1 and interactions as implied:" correctAtoms2And3 (solveBB 1 testInteractions2)
test2_3_1 = TestCase $ assertEqual "solveBB with grid 2 and interactions as implied:" [] (solveBB 2 testInteractions3_1)
test2_3_2 = TestCase $ assertEqual "solveBB with grid 2 and interactions as implied:" correctAtoms2And3 (solveBB 2 testInteractions3_2)
test2_3_3 = TestCase $ assertEqual "solveBB with grid 2 and interactions as implied:" correctAtoms2And3 (solveBB 2 testInteractions3_3)
test2_4 = TestCase $ assertEqual "solveBB with grid 3 and interactions as implied:" correctAtoms4 (solveBB 3 testInteractions4)
test2_5 = TestCase $ assertEqual "solveBB with grid 4 and interactions as implied:" correctAtoms5 (solveBB 4 testInteractions5)
test2_6 = TestCase $ assertEqual "solveBB with grid 4 and interactions as implied:" correctAtoms6 (solveBB 8 testInteractions6)

-- test from coursework spec with GRID SIZE 8
testInteractions1 :: Interactions
testInteractions1 = [((North,1),Path (West,2)), ((North,5),Path (East,5)), ((East,7),Path (East,4)), ((South,7),Absorb), ((West,1),Path (East,1)), ((West,3),Absorb) ]

-- test base case with GRID SIZE 1
testInteractions2 :: Interactions
testInteractions2 = [((North,1),Absorb)]

-- test cases with GRID SIZE 2 and atom at (1,1)
testInteractions3_1 :: Interactions
testInteractions3_1 = [((North,1),Absorb)]

testInteractions3_2 :: Interactions
testInteractions3_2 = [((West,2),Reflect)]

testInteractions3_3 :: Interactions
testInteractions3_3 = [((East,2),Path (South,2))]

-- test case with GRID SIZE 3 and atom at (2,2) and (2,3)
testInteractions4 :: Interactions
testInteractions4 = [((East,1),Path (North,3)) , ((East,3),Absorb) , ((South,3),Reflect)]

-- test case with GRID SIZE 4 and atom at (1,2) and (2,4) and (4,2)
testInteractions5 :: Interactions
testInteractions5 = [((West,1),Reflect) , ((South,1),Reflect) , ((North,3),Path (North,2)) , ((East,2),Absorb)]

-- test case with GRID SIZE 8 and atom at (2,2) and (4,2) and (3,3) and (7,2) and (8,7)
testInteractions6 :: Interactions
testInteractions6 = [((West,3),Path (South,1)) , ((North,3),Reflect) , ((East,2),Absorb) , ((South,5),Path (South,6)) , ((West,4),Path (South,2)) , ((East,8),Reflect)]


correctAtoms1 :: Atoms
correctAtoms1 = [ (2,3) , (4,6) , (7,3) , (7,8) ]

correctAtoms2And3 :: Atoms
correctAtoms2And3 = [(1,1)]

correctAtoms4 :: Atoms
correctAtoms4 = [ (2,2) , (2,3)]

correctAtoms5 :: Atoms
correctAtoms5 = [ (1,2) , (2,4) , (4,2) ]

correctAtoms6 :: Atoms
correctAtoms6 = [ (2,2) , (3,3) , (4,2) , (7,2) , (8,7)]



-- test3 for coursework question 3


tests3 = TestLabel "calcBBInteractions TESTS" $ TestList [test3_1, test3_2, test3_3, test3_4, test3_5, test3_6, test3_7]

test3_1 = TestCase $ assertEqual "prettyPrint with string as implied:" correctString1 (prettyPrint testExpr1)
test3_2 = TestCase $ assertEqual "prettyPrint with string as implied:" correctString2 (prettyPrint testExpr2)
test3_3 = TestCase $ assertEqual "prettyPrint with string as implied:" correctString3 (prettyPrint testExpr3)
test3_4 = TestCase $ assertEqual "prettyPrint with string as implied:" correctString4 (prettyPrint testExpr4)
test3_5 = TestCase $ assertEqual "prettyPrint with string as implied:" correctString5 (prettyPrint testExpr5)
test3_6 = TestCase $ assertEqual "prettyPrint with string as implied:" correctString6 (prettyPrint testExpr6)
test3_7 = TestCase $ assertEqual "prettyPrint with string as implied:" correctString7 (prettyPrint testExpr7)

testExpr1 :: LamExpr
testExpr1 = LamApp (LamAbs 1 (LamVar 1)) (LamAbs 1 (LamVar 1))

testExpr2 :: LamExpr
testExpr2 = LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamVar 1)))

testExpr3 :: LamExpr
testExpr3 = LamApp (LamVar 2) (LamAbs 1 (LamAbs 2 (LamVar 1)))

testExpr4 :: LamExpr
testExpr4 = LamAbs 1 (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamAbs 2 (LamVar 1)))))

testExpr5 :: LamExpr
testExpr5 = LamAbs 3 (LamAbs 2 (LamApp (LamVar 3) (LamVar 2)))

testExpr6 :: LamExpr
testExpr6 = LamAbs 0 (LamAbs 1 (LamVar 0))

testExpr7 :: LamExpr
testExpr7 = LamAbs 1 (LamAbs 0 (LamVar 1))

testExpr8 :: LamExpr
testExpr8 = LamAbs 1 (LamAbs 0 (LamVar 0))

testExpr9 :: LamExpr
testExpr9 = LamAbs 0 (LamAbs 1 (LamAbs 2 (LamVar 0)))


correctString1 :: String
correctString1 = "(\\x0 -> x0) \\x0 -> x0"

correctString2 :: String
correctString2 = "\\x0 -> x0 \\x0 -> x0"

correctString3 :: String
correctString3 = "x2 \\x0 -> \\x1 -> x0"

correctString4 :: String
correctString4 = "\\x0 -> \\x0 -> x0 \\x0 -> \\x1 -> x0"

correctString5 :: String
correctString5 = "\\x1 -> \\x0 -> x1 x0"

correctString6 :: String
correctString6 = "\\x0 -> \\x1 -> x0"

correctString7 :: String
correctString7 = "\\x0 -> \\x1 -> x0"

correctString8 :: String
correctString8 = "\\x0 -> \\x0 -> x0"

correctString9 :: String
correctString9 = "\\x0 -> \\x1 -> \\x1 -> x0"




-- test4 for coursework question 4


tests4 = TestLabel "calcBBInteractions TESTS" $ TestList [test4_1, test4_2, test4_3, test4_4, test4_5, test4_6, test4_7, test4_8]

test4_1 = TestCase $ assertEqual "parseArith with string as implied:" correctArithExpr1 (parseArith testString1)
test4_2 = TestCase $ assertEqual "parseArith with string as implied:" correctArithExpr2 (parseArith testString2)
test4_3 = TestCase $ assertEqual "parseArith with string as implied:" correctArithExpr3 (parseArith testString3)
test4_4 = TestCase $ assertEqual "parseArith with string as implied:" correctArithExpr4 (parseArith testString4)
test4_5 = TestCase $ assertEqual "parseArith with string as implied:" correctArithExpr5 (parseArith testString5)
test4_6 = TestCase $ assertEqual "parseArith with string as implied:" correctArithExpr6 (parseArith testString6)
test4_7 = TestCase $ assertEqual "parseArith with string as implied:" correctArithExpr7 (parseArith testString7)
test4_8 = TestCase $ assertEqual "parseArith with string as implied:" correctArithExpr8 (parseArith testString8)

testString1 :: String
testString1 = "1 + 2"

testString2 :: String
testString2 = "(+1) 2"

testString3 :: String
testString3 = "2 (+1)"

testString4 :: String
testString4 = "(+1) (+2) 3"

testString5 :: String
testString5 = " ( + ( 5 * 2 ) ) 6 + 7 "

testString6 :: String
testString6 = "1 . 2"

testString7 :: String
testString7 = "(4 + 5) * 3"

testString8 :: String
testString8 = ""


correctArithExpr1 :: Maybe ArithExpr
correctArithExpr1 = Just (Add (ArithNum 1) (ArithNum 2))

correctArithExpr2 :: Maybe ArithExpr
correctArithExpr2 = Just (SecApp (Section (ArithNum 1)) (ArithNum 2))

correctArithExpr3 :: Maybe ArithExpr
correctArithExpr3 = Nothing

correctArithExpr4 :: Maybe ArithExpr
correctArithExpr4 = Just (SecApp (Section (ArithNum 1)) (SecApp (Section (ArithNum 2)) (ArithNum 3)))

correctArithExpr5 :: Maybe ArithExpr
correctArithExpr5 = Just (Add ( SecApp ( Section ( Mul ( ArithNum 5 ) ( ArithNum 2 ))) ( ArithNum 6 )) ( ArithNum 7 ) )

correctArithExpr6 :: Maybe ArithExpr
correctArithExpr6 = Nothing

correctArithExpr7 :: Maybe ArithExpr
correctArithExpr7 = Just (Mul (Add (ArithNum 4) (ArithNum 5)) (ArithNum 3) )

correctArithExpr8 :: Maybe ArithExpr
correctArithExpr8 = Nothing




-- test5 for coursework question 5


tests5 = TestLabel "calcBBInteractions TESTS" $ TestList [test5_1, test5_2, test5_3, test5_4, test5_5, test5_6]

test5_1 = TestCase $ assertEqual "churchEnc with Arithmetic Expression as implied:" correctLamExpr1 (churchEnc testArithExpr1)
test5_2 = TestCase $ assertEqual "churchEnc with Arithmetic Expression as implied:" correctLamExpr2 (churchEnc testArithExpr2)
test5_3 = TestCase $ assertEqual "churchEnc with Arithmetic Expression as implied:" correctLamExpr3 (churchEnc testArithExpr3)
test5_4 = TestCase $ assertEqual "churchEnc with Arithmetic Expression as implied:" correctLamExpr4 (churchEnc testArithExpr4)
test5_5 = TestCase $ assertEqual "churchEnc with Arithmetic Expression as implied:" correctLamExpr5 (churchEnc testArithExpr5)
test5_6 = TestCase $ assertEqual "churchEnc with Arithmetic Expression as implied:" correctLamExpr6 (churchEnc testArithExpr6)


testArithExpr1 :: ArithExpr
testArithExpr1 = (ArithNum 2)

testArithExpr2 :: ArithExpr
testArithExpr2 = (SecApp (Section (ArithNum 1)) (ArithNum 1))

testArithExpr3 :: ArithExpr
testArithExpr3 = (Add (ArithNum 2) (ArithNum 2))

testArithExpr4 :: ArithExpr
testArithExpr4 = (Mul (ArithNum 3) (ArithNum 1))

testArithExpr5 :: ArithExpr
testArithExpr5 = (Mul (ArithNum 3) (Add (ArithNum 2) (ArithNum 2)))

testArithExpr6 :: ArithExpr
testArithExpr6 = (Add ( SecApp ( Section ( Mul ( ArithNum 3 ) ( ArithNum 2 ))) ( ArithNum 1 )) ( ArithNum 4 ) )


correctLamExpr1 :: LamExpr
correctLamExpr1 = LamAbs 0 (LamAbs 1 ( LamApp (LamVar 0) (LamApp (LamVar 0)(LamVar 1)) ) )

correctLamExpr2 :: LamExpr
correctLamExpr2 = LamApp (LamApp ( ePlus ) ( e1 )) ( e1 )
    where 
       e1 = LamAbs 0 (LamAbs 1 (LamApp (LamVar 0) (LamVar 1)))
       ePlus = LamAbs 0 (LamAbs 1 ( LamAbs 2 ( LamAbs 3 ( LamApp ( LamApp (LamVar 0 ) (LamVar 2 ) ) ( LamApp (LamApp (LamVar 1) (LamVar 2)) ( LamVar 3) ) ) ) ) ) 

correctLamExpr3 :: LamExpr
correctLamExpr3 = LamApp (LamApp ( ePlus ) ( e2 )) ( e2 )
    where 
       e2 = LamAbs 0 (LamAbs 1 ( LamApp (LamVar 0) (LamApp (LamVar 0)(LamVar 1)) ) )
       ePlus = LamAbs 0 (LamAbs 1 ( LamAbs 2 ( LamAbs 3 ( LamApp ( LamApp (LamVar 0 ) (LamVar 2 ) ) ( LamApp (LamApp (LamVar 1) (LamVar 2)) ( LamVar 3) ) ) ) ) )

correctLamExpr4 :: LamExpr
correctLamExpr4 = LamApp (LamApp ( eMul ) ( e3 )) ( e1 )
    where 
       e1 = LamAbs 0 (LamAbs 1 (LamApp (LamVar 0) (LamVar 1)))
       e3 = LamAbs 0 (LamAbs 1 ( LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0)(LamVar 1)) ) ) )
       eMul = LamAbs 0 (LamAbs 1 ( LamAbs 2 ( LamAbs 3 ( LamApp ( LamApp (LamVar 0) (LamApp (LamVar 1) (LamVar 2))) (LamVar 3)) ) ) )

correctLamExpr5 :: LamExpr
correctLamExpr5 = LamApp (LamApp ( eMul ) (e3)) (LamApp (LamApp ( ePlus ) ( e2 )) ( e2 ) )
    where 
       e2 = LamAbs 0 (LamAbs 1 ( LamApp (LamVar 0) (LamApp (LamVar 0)(LamVar 1)) ) )
       e3 = LamAbs 0 (LamAbs 1 ( LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0)(LamVar 1)) ) ) )
       ePlus = LamAbs 0 (LamAbs 1 ( LamAbs 2 ( LamAbs 3 ( LamApp ( LamApp (LamVar 0 ) (LamVar 2 ) ) ( LamApp (LamApp (LamVar 1) (LamVar 2)) ( LamVar 3) ) ) ) ) )
       eMul = LamAbs 0 (LamAbs 1 ( LamAbs 2 ( LamAbs 3 ( LamApp ( LamApp (LamVar 0) (LamApp (LamVar 1) (LamVar 2))) (LamVar 3)) ) ) )

correctLamExpr6 :: LamExpr
correctLamExpr6 =  LamApp (LamApp (ePlus) (LamApp (LamApp ( ePlus ) (LamApp (LamApp ( eMul ) ( e3 )) ( e2 ) )) (e1) )) (e4)
    where 
       e1 = LamAbs 0 (LamAbs 1 ( LamApp (LamVar 0) (LamVar 1)))
       e2 = LamAbs 0 (LamAbs 1 ( LamApp (LamVar 0) (LamApp (LamVar 0) (LamVar 1))))
       e3 = LamAbs 0 (LamAbs 1 ( LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamVar 1)))))
       e4 = LamAbs 0 (LamAbs 1 ( LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamVar 1))))))
       ePlus = LamAbs 0 (LamAbs 1 ( LamAbs 2 ( LamAbs 3 ( LamApp ( LamApp (LamVar 0 ) (LamVar 2 ) ) ( LamApp (LamApp (LamVar 1) (LamVar 2)) ( LamVar 3) ) ) ) ) )
       eMul = LamAbs 0 (LamAbs 1 ( LamAbs 2 ( LamAbs 3 ( LamApp ( LamApp (LamVar 0) (LamApp (LamVar 1) (LamVar 2))) (LamVar 3)) ) ) )


-- test6 for coursework question 6


tests6 = TestLabel "calcBBInteractions TESTS" $ TestList [test6_1, test6_2, test6_3, test6_4, test6_5, test6_6, test6_7, test6_8, test6_9, test6_10, test6_11, test6_12, test6_13, test6_14]

test6_1 = TestCase $ assertEqual "checking reduction of Lamda Expression is correct:" correctQ6LamExpr1 (reduceUntilLam (churchEnc testQ6ArithExpr1))
test6_2 = TestCase $ assertEqual "checking reduction of Lamda Expression is correct:" correctQ6LamExpr2 (reduceUntilLam (churchEnc testQ6ArithExpr2))
test6_3 = TestCase $ assertEqual "checking reduction of Lamda Expression is correct:" correctQ6LamExpr3 (reduceUntilLam (churchEnc testQ6ArithExpr3))
test6_4 = TestCase $ assertEqual "checking reduction of Lamda Expression is correct:" correctQ6LamExpr4 (reduceUntilLam (churchEnc testQ6ArithExpr4))
test6_5 = TestCase $ assertEqual "checking reduction of Arithmetic Expression is correct:" correctQ6ArithExpr1 (reduceUntilArith testQ6ArithExpr1)
test6_6 = TestCase $ assertEqual "checking reduction of Arithmetic Expression is correct:" correctQ6ArithExpr2 (reduceUntilArith testQ6ArithExpr2)
test6_7 = TestCase $ assertEqual "checking reduction of Arithmetic Expression is correct:" correctQ6ArithExpr3 (reduceUntilArith testQ6ArithExpr3)
test6_8 = TestCase $ assertEqual "checking reduction of Arithmetic Expression is correct:" correctQ6ArithExpr4 (reduceUntilArith testQ6ArithExpr4)
test6_9 = TestCase $ assertEqual "compareArithLam with Arithmetic Expression as implied:" correctPair1 (compareArithLam testQ6ArithExpr1)
test6_10 = TestCase $ assertEqual "compareArithLam with Arithmetic Expression as implied:" correctPair2 (compareArithLam testQ6ArithExpr2)
test6_11 = TestCase $ assertEqual "compareArithLam with Arithmetic Expression as implied:" correctPair3 (compareArithLam testQ6ArithExpr3)
test6_12 = TestCase $ assertEqual "compareArithLam with Arithmetic Expression as implied:" correctPair4 (compareArithLam testQ6ArithExpr4)
test6_13 = TestCase $ assertEqual "compareArithLam with Arithmetic Expression as implied:" correctPair5 (compareArithLam testQ6ArithExpr5)
test6_14 = TestCase $ assertEqual "compareArithLam with Arithmetic Expression as implied:" correctPair6 (compareArithLam testQ6ArithExpr6)



reduceUntilLam :: LamExpr -> LamExpr
reduceUntilLam e = if innerRedn1 e == Nothing then e else reduceUntilLam $ evalLamRedn1 $ innerRedn1 e

evalLamRedn1 :: Maybe LamExpr -> LamExpr
evalLamRedn1 (Just e) = e


reduceUntilArith :: ArithExpr -> ArithExpr
reduceUntilArith e = if innerArithRedn1 e == Nothing then e else reduceUntilArith $ evalArithRedn1 $ innerArithRedn1 e

evalArithRedn1 :: Maybe ArithExpr -> ArithExpr
evalArithRedn1 (Just e) = e


testQ6ArithExpr1 :: ArithExpr
testQ6ArithExpr1 = (ArithNum 4)

testQ6ArithExpr2 :: ArithExpr
testQ6ArithExpr2 = (Add (ArithNum 4) (ArithNum 5))

testQ6ArithExpr3 :: ArithExpr
testQ6ArithExpr3 = (Mul (Add (ArithNum 4) (ArithNum 5)) (ArithNum 3) )

testQ6ArithExpr4 :: ArithExpr
testQ6ArithExpr4 = (Mul (Add (SecApp (Section (ArithNum 1)) (ArithNum 4)) (ArithNum 5)) (ArithNum 3))

testQ6ArithExpr5 :: ArithExpr
testQ6ArithExpr5 = (Mul (ArithNum 4) (ArithNum 5))

testQ6ArithExpr6 :: ArithExpr
testQ6ArithExpr6 = (SecApp (Section (ArithNum 5)) (Add (SecApp (Section (ArithNum 1)) (ArithNum 4)) (Mul (ArithNum 4) (ArithNum 2))))


correctQ6ArithExpr1 :: ArithExpr
correctQ6ArithExpr1 = (ArithNum 4)

correctQ6ArithExpr2 :: ArithExpr
correctQ6ArithExpr2 = (ArithNum 9)

correctQ6ArithExpr3 :: ArithExpr
correctQ6ArithExpr3 = (ArithNum 27)

correctQ6ArithExpr4 :: ArithExpr
correctQ6ArithExpr4 = (ArithNum 30)


correctQ6LamExpr1 :: LamExpr
correctQ6LamExpr1 = LamAbs 0 (LamAbs 1 (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamVar 1))))))

correctQ6LamExpr2 :: LamExpr
correctQ6LamExpr2 = LamAbs 0 (LamAbs 1 (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamVar 1)))))))))))

correctQ6LamExpr3 :: LamExpr
correctQ6LamExpr3 = LamAbs 0 (LamAbs 1 (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamVar 1)))))))))))))))))))))))))))))

correctQ6LamExpr4 :: LamExpr
correctQ6LamExpr4 = LamAbs 0 (LamAbs 1 (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamVar 1))))))))))))))))))))))))))))))))


correctPair1 :: (Int,Int)
correctPair1 = (0,0)

correctPair2 :: (Int,Int)
correctPair2 = (1,6)

correctPair3 :: (Int,Int)
correctPair3 = (2,28)

correctPair4 :: (Int,Int)
correctPair4 = (3,36)

correctPair5 :: (Int,Int)
correctPair5 = (1,12)

correctPair6 :: (Int,Int)
correctPair6 = (4,30)


