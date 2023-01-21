{-# LANGUAGE DeriveGeneric #-}
-- comp2209 Functional Programming Challenges
-- (c) University of Southampton 2021
-- Skeleton code to be updated with your solutions
-- The dummy functions here simply return an arbitrary value that is usually wrong 

-- DO NOT MODIFY THE FOLLOWING LINES OF CODE
module Challenges (Atoms,Interactions,Pos,EdgePos,Side(..),Marking(..),
                   LamExpr(..),ArithExpr(..),
                   calcBBInteractions,
                   solveBB,
                   prettyPrint,
                   parseArith,
                   churchEnc,
                   innerRedn1,innerArithRedn1,compareArithLam)
where


-- Import standard library and parsing definitions from Hutton 2016, Chapter 13
import Data.Char
import Parsing
import Control.Monad
import Data.List
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq

instance NFData ArithExpr
instance NFData LamExpr 
instance NFData Marking
instance NFData Side


-- Challenge 1
-- Calculate Interactions in the Black Box

type Atoms = [ Pos ]
type Interactions = [  ( EdgePos , Marking )  ] 
type Pos = (Int, Int)   -- top left is (1,1) , bottom right is (N,N) where N is size of grid
type EdgePos = ( Side , Int ) -- int range is 1 to N where N is size of grid

data Side = North | East | South | West
            deriving (Show, Eq, Ord)

data Marking =  Absorb | Reflect | Path EdgePos
                deriving (Show, Eq)



calcBBInteractions :: Int -> Atoms -> [EdgePos] -> Interactions
calcBBInteractions n atms xs = checkForReflections $ calcBBInteractions' n atms xs

-- calculates the interactions between atoms and the edge fired rays inside a black box of size n
calcBBInteractions' :: Int -> Atoms -> [EdgePos] -> Interactions
calcBBInteractions' _ _ [] = []
calcBBInteractions' _ [] edgePos = calcNoAtomsIntercs edgePos  -- if there are not atoms present in the black box, all interactions are straight fired rays
calcBBInteractions' n atms ((s,x):ys)
   | reflectAtomCheck n atms (s,x) == True = ((s,x),Reflect) : calcBBInteractions' n atms ys  -- first checks for any reflections with boundary atoms
   | absorbAtomCheck n atms atms (s,x) == True = ((s,x),Absorb) : calcBBInteractions' n atms ys  -- afterwards, checks for any absorbing atoms
   | changePathCheck atms atms (s,x) == True = ((s,x), fireRay n atms atms (s,x) (findPos n (s,x))) : calcBBInteractions' n atms ys  --finally, checks for a change in the path of the ray
   | otherwise = ((s,x),oppositeMarking (s,x)) : calcBBInteractions' n atms ys  --if none of these are satisfied, it just shoots a ray straight through the box

-- function checks to make sure, any fired rays which enter and exit the black box at the same place are renamed to Reflect Marking
checkForReflections :: Interactions -> Interactions
checkForReflections [] = []
checkForReflections (((s1,x1), Path(s2,x2)):xs)
   | s1 == s2 && x1 == x2 = ((s1,x1),Reflect) : checkForReflections xs
   | otherwise = ((s1,x1), Path(s2,x2)) : checkForReflections xs
checkForReflections (x:xs) = x : checkForReflections xs

-- function calculates all interactions that happen when no atoms are in the black box
calcNoAtomsIntercs :: [EdgePos] -> Interactions
calcNoAtomsIntercs [] = []
calcNoAtomsIntercs ((s,x):xs) = ((s,x),oppositeMarking (s,x)) : calcNoAtomsIntercs xs

-- function checks to see if a fired ray should reflect with the atoms on the border of the black box
reflectAtomCheck :: Int -> Atoms -> EdgePos -> Bool
reflectAtomCheck _ [] _ = False
reflectAtomCheck n ((x,y):at) (s,z)
   | s == North && y == 1 && (x == z+1 || x == z-1) = True
   | s == South && y == n && (x == z+1 || x == z-1) = True
   | s == East && x == n &&  (y == z+1 || y == z-1) = True
   | s == West && x == 1 &&  (y == z+1 || y == z-1) = True
   | otherwise = reflectAtomCheck n at (s,z)

-- this function checks to see if an atom is in the path of a fired ray and an absorption should happen and returns a True boolean if yes, or False if no
--  if an atom lies in the path of the ray, the function checks to make sure there are no atoms on either side of the fired ray which may change its path
absorbAtomCheck :: Int -> Atoms -> Atoms -> EdgePos -> Bool
absorbAtomCheck _ [] _ _ = False
absorbAtomCheck n ((x,y):at) atms (s,z)
   | s == East  && y == z = if (obstrAtomE n (x,y) atms) == False then checkNoPathE n (x,y) atms else absorbAtomCheck n at atms (s,z)
   | s == West  && y == z = if (obstrAtomW (x,y) atms)   == False then checkNoPathW (x,y) atms   else absorbAtomCheck n at atms (s,z)
   | s == North && x == z = if (obstrAtomN (x,y) atms)   == False then checkNoPathN (x,y) atms   else absorbAtomCheck n at atms (s,z)
   | s == South && x == z = if (obstrAtomS n (x,y) atms) == False then checkNoPathS n (x,y) atms else absorbAtomCheck n at atms (s,z)
   | otherwise = absorbAtomCheck n at atms (s,z)

-- these 4 functions all check to see if there is an atom in the path of a fired ray
obstrAtomE :: Int -> Pos -> Atoms -> Bool
obstrAtomE n (x,y) atms
   | n == x = False
   | ((x+1),y) `elem` atms = True
   | otherwise = obstrAtomE n ((x+1),y) atms

obstrAtomW :: Pos -> Atoms -> Bool
obstrAtomW (0,y) _ = False
obstrAtomW (x,y) atms
   | ((x-1),y) `elem` atms = True
   | otherwise = obstrAtomW ((x-1),y) atms

obstrAtomN :: Pos -> Atoms -> Bool
obstrAtomN (x,0) _ = False
obstrAtomN (x,y) atms
   | (x,(y-1)) `elem` atms = True
   | otherwise = obstrAtomN (x,(y-1)) atms

obstrAtomS :: Int -> Pos -> Atoms -> Bool
obstrAtomS n (x,y) atms
   | n == y = False
   | (x,(y+1)) `elem` atms = True
   | otherwise = obstrAtomS n (x,(y+1)) atms

-- these 4 functions all check to make sure there are no atoms on either side of a fired ray, in case there is, the ray needs to change path and NOT be absorbed
checkNoPathE :: Int -> Pos -> Atoms -> Bool
checkNoPathE n (x,y) atms
   | n == x = True
   | ((x+1),(y-1)) `elem` atms || ((x+1),(y+1)) `elem` atms = False
   | otherwise = checkNoPathE n ((x+1),y) atms

checkNoPathW :: Pos -> Atoms -> Bool
checkNoPathW (0,y) _ = True
checkNoPathW (x,y) atms
   | ((x-1),(y-1)) `elem` atms || ((x-1),(y+1)) `elem` atms = False
   | otherwise = checkNoPathW ((x-1),y) atms

checkNoPathN :: Pos -> Atoms -> Bool
checkNoPathN (x,0) _ = True
checkNoPathN (x,y) atms
   | ((x-1),(y-1)) `elem` atms || ((x+1),(y-1)) `elem` atms = False
   | otherwise = checkNoPathN (x,(y-1)) atms

checkNoPathS :: Int -> Pos -> Atoms -> Bool
checkNoPathS n (x,y) atms
   | n == y = True
   | ((x-1),(y+1)) `elem` atms || ((x+1),(y+1)) `elem` atms = False
   | otherwise = checkNoPathS n (x,(y+1)) atms

-- this function checks to see if there is an atom on either side of the fired ray into the box, in which case the path of the ray needs to change
changePathCheck :: Atoms -> Atoms -> EdgePos -> Bool
changePathCheck [] _ _ = False
changePathCheck ((x,y):at) atms (s,z)
   | (s == North || s == South) && (x == z+1 || x == z-1) = True
   | (s == East || s == West)   && (y == z+1 || y == z-1) = True
   | otherwise = changePathCheck at atms (s,z)

-- this function changes the path of the ray depending on the positions of the atoms, and checks to see if the new path is absorbed; changed or exits the black box
fireRay :: Int -> Atoms -> Atoms -> EdgePos -> Pos -> Marking
fireRay n [] atms (s,z) (a,b)
   | s == South = fireRay n atms atms (s,z) (a,b-1)
   | s == North = fireRay n atms atms (s,z) (a,b+1)
   | s == West = fireRay n atms atms (s,z) (a+1,b)
   | s == East = fireRay n atms atms (s,z) (a-1,b)
fireRay n ((x,y):xys) atms (s,z) (a,b)
   | pathAbsorbCheck (s,z) (a,b) atms == True = Absorb
   | checkEdgeCase n s (a,b) == True = findPath s (a,b)
   | s == North && (a-1,b+1) == (x,y) = if (a+1,b+1) `elem` atms then fireRay n atms atms (South,b) (a,b) else fireRay n atms atms (West,a) ((a+1),b)
   | s == North && (a+1,b+1) == (x,y) = if (a-1,b+1) `elem` atms then fireRay n atms atms (South,b) (a,b) else fireRay n atms atms (East,a) ((a+1),b)
   | s == South && (a-1,b-1) == (x,y) = if (a+1,b-1) `elem` atms then fireRay n atms atms (North,b) (a,b) else fireRay n atms atms (West,a) ((a+1),b)
   | s == South && (a+1,b-1) == (x,y) = if (a-1,b-1) `elem` atms then fireRay n atms atms (North,b) (a,b) else fireRay n atms atms (East,a) ((a-1),b)
   | s == East && (a-1,b+1)  == (x,y) = if (a-1,b-1) `elem` atms then fireRay n atms atms (West,a) (a,b)  else fireRay n atms atms (South,a) (a,(b-1))
   | s == East && (a-1,b-1)  == (x,y) = if (a-1,b+1) `elem` atms then fireRay n atms atms (West,a) (a,b)  else fireRay n atms atms (North,a) (a,(b+1))
   | s == West && (a+1,b+1)  == (x,y) = if (a+1,b-1) `elem` atms then fireRay n atms atms (East,a) (a,b)  else fireRay n atms atms (South,a) (a,(b-1))
   | s == West && (a+1,b-1)  == (x,y) = if (a+1,b+1) `elem` atms then fireRay n atms atms (East,a) (a,b)  else fireRay n atms atms (North,a) (a,(b+1))
   | otherwise = fireRay n xys atms (s,z) (a,b)

-- this function is used to check if the ray fired into the box, after changing directions, is absorbed by an atom or not
pathAbsorbCheck :: EdgePos -> Pos -> Atoms -> Bool
pathAbsorbCheck (s,z) (a,b) atms
   | s == North && (a,b+1) `elem` atms = True
   | s == South && (a,b-1) `elem` atms = True
   | s == East && (a-1,b) `elem` atms = True
   | s == North && (a+1,b) `elem` atms = True
   | otherwise = False

-- this function checks to see of the ray exits the black box at a specific point
checkEdgeCase :: Int -> Side -> Pos -> Bool
checkEdgeCase n s (a,b)
   | s == North && b == (n+1) = True
   | s == South && b == 0 = True
   | s == East  && a == 0 = True
   | s == West  && a == (n+1) = True
   | otherwise = False

-- this function gives the Marking of a ray that has exited the black box. The markings are inversed, as the ray travels in the opposite drection of the labelled side of the black box
findPath :: Side -> Pos -> Marking
findPath North (a,b) = Path (South, a)
findPath South (a,b) = Path (North, a)
findPath East (a,b) = Path (West, b)
findPath West (a,b) = Path (East, b)

-- this function finds the position of the ray which is initally fired into the black box.  for example, a ray fired into a black box of size n = 3 at (East,3) will have the coordinate (3,3)
findPos :: Int -> EdgePos -> Pos
findPos n (North,x) = (x,1)
findPos n (South,x) = (x,n)
findPos n (East,x) = (n,x)
findPos n (West,x) = (1,x)

-- this function calculates the rays path if it doesn't interact with any atoms and just exits the black box on the opposite side of where it was fired
oppositeMarking :: EdgePos -> Marking
oppositeMarking (North,n) = Path (South,n)
oppositeMarking (South,n) = Path (North,n)
oppositeMarking (East,n) = Path (West,n)
oppositeMarking (West,n) = Path (East,n)



-- Challenge 2
-- Solve a Black Box from partial interactions

-- returns a minimal list of positions of atoms that could be in the black box, given the interactions
solveBB :: Int -> Interactions -> Atoms
solveBB _ [] = []
solveBB 1 xs = [(1,1)]
solveBB n xs = rmZeros $ rmDuplicates $ findAtomRep xs ++ findSameAtomRep (findAtomRep xs ++ calculateReflRep n xs xs) xs xs ++ calculateReflRep n xs xs

-- this function removes any atoms that could not be found
rmZeros :: Atoms -> Atoms
rmZeros [] = []
rmZeros ((x,y):xys)
   | x == 0 || y == 0  = rmZeros xys
   | otherwise = (x,y) : rmZeros xys

-- recursive function to find any atoms that change the path of the fired ray once. these atoms are easy to find, as there is most probably only one solution to this interaction
findAtomRep :: Interactions -> Atoms
findAtomRep [] = []
findAtomRep (x:xs) = findAtom x ++ findAtomRep xs

-- calculates the atom's position, depending on where the reflected ray changed direction to
findAtom :: (EdgePos,Marking) -> Atoms
findAtom ((North,x1),Path(West,x2)) = [(x1+1,x2+1)]
findAtom ((North,x1),Path(East,x2)) = [(x1-1,x2+1)]
findAtom ((South,x1),Path(West,x2)) = [(x1+1,x2-1)]
findAtom ((South,x1),Path(East,x2)) = [(x1-1,x2-1)]
findAtom ((East,x1),Path(North,x2)) = [(x2-1,x1+1)]
findAtom ((East,x1),Path(South,x2)) = [(x2-1,x1-1)]
findAtom ((West,x1),Path(North,x2)) = [(x2+1,x1+1)]
findAtom ((West,x1),Path(South,x2)) = [(x2+1,x1-1)]
findAtom x = []

-- recursive function that finds any atoms which cause a double change of path, back to the original side in which the way was fired (e.g. ((East,7),Path (East,4)))
findSameAtomRep :: Atoms -> Interactions -> Interactions -> Atoms
findSameAtomRep _ [] _ = []
findSameAtomRep atms (x:xs) ys = findSameAtom atms x ys ++ findSameAtomRep atms xs ys

-- tries to calculate the two atoms' positions based on existing knowledge of already found atoms OR on interactions that could give the atom's positions (e.g. Absorb, Reflect).  the function does this by checking for x or y coordinates of other atoms that could match with the reflections generated by the double path change.
findSameAtom :: Atoms -> (EdgePos,Marking) -> Interactions -> Atoms
findSameAtom atms ((dir1,x1),Path(dir2,x2)) xs
   | dir1 == dir2 && (dir1 == East || dir1 == West) = 
        if x1 > x2 
           then [ (findX ((x1+1),(x2-1)) atms xs , (x1+1))  ,  (findX ((x1+1),(x2-1)) atms xs , (x2-1)) ] 
           else [ (findX ((x2+1),(x1-1)) atms xs , (x2+1))  ,  (findX ((x2+1),(x1-1)) atms xs , (x1-1)) ]
   | dir1 == dir2 && (dir1 == North || dir1 == South) = 
       if x1 > x2 
           then [( (x1+1) , findY ((x1+1),(x2-1)) atms xs)  ,  ((x2-1) , findY ((x1+1),(x2-1)) atms xs) ] 
           else [( (x2+1) , findY ((x2+1),(x1-1)) atms xs)  ,  ((x1-1) , findY ((x2+1),(x1-1)) atms xs) ]
   | otherwise = []
findSameAtom atms ys xs = []

-- this function tries to find the x-coordinate of the doubled path change which starts at either East or West side using any Absorb interactions which may hint at where the atoms are
findX :: Pos -> Atoms -> Interactions -> Int
findX (x,y) atms [] = checkEWExistAtoms (x,y) atms
findX (x,y) atms (((dir1,n),Absorb):xs)
   | (dir1 == North || dir1 == South) = n
   | otherwise = findX (x,y) atms xs
findX (x,y) atms (z:zs) = findX (x,y) atms zs

-- this function tries to find the y-coordinate of the doubled path change which starts at either North or South side using any Absorb interactions which may hint at where the atoms are
findY :: Pos -> Atoms -> Interactions -> Int
findY (x,y) atms [] = checkNSExistAtoms (x,y) atms
findY (x,y) atms (((dir1,n),Absorb):xs)
   | (dir1 == East || dir1 == West) = n
   | otherwise = findY (x,y) atms xs
findY (x,y) atms (z:zs) = findY (x,y) atms zs

-- this function tries to find the x-coordinate of the doubled path change which starts at either East or West side using any known existing atoms and if no atoms can be found, adds a 0 instead as a coordinate, to erase this coordinate
checkEWExistAtoms :: Pos -> Atoms -> Int
checkEWExistAtoms (x,y) [] = 0
checkEWExistAtoms (x,y) ((a,b):abz)
   | x == b || y == b = a
   | otherwise = checkEWExistAtoms (x,y) abz

-- this function tries to find the y-coordinate of the doubled path change which starts at either North or South side using any known existing atoms and if no atoms can be found, adds a 0 instead as a coordinate, to erase this coordinate
checkNSExistAtoms :: Pos -> Atoms -> Int
checkNSExistAtoms (x,y) [] = 0
checkNSExistAtoms (x,y) ((a,b):abz)
   | x == a || y == a = b
   | otherwise = checkNSExistAtoms (x,y) abz

-- recursive function that finds any atoms which have a "reflect" as their interaction
calculateReflRep :: Int -> Interactions -> Interactions -> Atoms
calculateReflRep _ [] _ = []
calculateReflRep n (x:xs) ys = calculateRefl x n ys ++ calculateReflRep n xs ys

-- function to calculate atom's position due to a reflection. Reflections on the very edges of the board always show where atoms are, other reflections need to be found using existing knowledge of other atoms OR other interactions
calculateRefl :: (EdgePos,Marking) -> Int -> Interactions -> Atoms
calculateRefl ((dir,x),Reflect) n xs
   | dir == East && x == 1 = [(n,2)]
   | dir == East && x == n = [(n,(n-1))]
   | dir == West && x == 1 = [(1,2)]
   | dir == West && x == n = [(1,(n-1))]
   | dir == North && x == 1 = [(2,1)]
   | dir == North && x == n = [((n-1),1)]
   | dir == South && x == 1 = [(2,n)]
   | dir == South && x == n = [((n-1),n)]
   | dir == East = checkEastRefl x n xs
   | dir == West = checkWestRefl x n xs
   | dir == North = checkNorthRefl x n xs
   | dir == South = checkSouthRefl x n xs
calculateRefl y n xs = []

-- these four functions check to see if the reflection is shown to be a double reflection, and find two atoms that perhaps correspond to this, or a single reflection and try and find which side of the reflection the atom could be on
checkEastRefl:: Int -> Int -> Interactions -> Atoms
checkEastRefl x n ys
   | ((East,(x-1)),Absorb) `elem` ys && ((East,(x+1)),Absorb) `elem` ys = checkEWDoubleAtom x n ys
   | ((East,(x-1)),Absorb) `elem` ys = [(n,(x-1))]
   | ((East,(x+1)),Absorb) `elem` ys = [(n,(x+1))]
   | otherwise = []

checkWestRefl:: Int -> Int -> Interactions -> Atoms
checkWestRefl x n ys
   | ((West,(x-1)),Absorb) `elem` ys && ((West,(x+1)),Absorb) `elem` ys = checkEWDoubleAtom x n ys
   | ((West,(x-1)),Absorb) `elem` ys = [(1,(x-1))]
   | ((West,(x+1)),Absorb) `elem` ys = [(1,(x+1))]
   | otherwise = []

checkNorthRefl:: Int -> Int -> Interactions -> Atoms
checkNorthRefl x n ys
   | ((North,(x-1)),Absorb) `elem` ys && ((North,(x+1)),Absorb) `elem` ys = checkNSDoubleAtom x n ys
   | ((North,(x-1)),Absorb) `elem` ys = [((x-1),1)]
   | ((North,(x+1)),Absorb) `elem` ys = [((x+1,1))]
   | otherwise = []

checkSouthRefl:: Int -> Int -> Interactions -> Atoms
checkSouthRefl x n ys
   | ((South,(x-1)),Absorb) `elem` ys && ((South,(x+1)),Absorb) `elem` ys = checkNSDoubleAtom x n ys
   | ((South,(x-1)),Absorb) `elem` ys = [((x-1),n)]
   | ((South,(x+1)),Absorb) `elem` ys = [((x+1,n))]
   | otherwise = []

-- these functions try and find the two atoms that could result in the double reflection
checkEWDoubleAtom :: Int -> Int -> Interactions -> Atoms
checkEWDoubleAtom x n ys
   | ((East,(x-2)),Reflect) `elem` ys && ((East,(x+2)),Reflect) `elem` ys = [(n,(x-1)),(n,(x+1))]
   | ((West,(x-2)),Reflect) `elem` ys && ((West,(x+2)),Reflect) `elem` ys = [(1,(x-1)),(1,(x+1))]
   | ((East,(x-2)),Reflect) `elem` ys = [(n,(x-1))]
   | ((West,(x-2)),Reflect) `elem` ys = [(1,(x-1))]
   | ((East,(x+2)),Reflect) `elem` ys = [(n,(x+1))]
   | ((West,(x+2)),Reflect) `elem` ys = [(1,(x+1))]
   | otherwise = findESDoubleRefl x (findAtomRep ys ++ findSameAtomRep (findAtomRep ys) ys ys)

checkNSDoubleAtom :: Int -> Int -> Interactions -> Atoms
checkNSDoubleAtom x n ys
   | ((North,(x-2)),Reflect) `elem` ys && ((North,(x+2)),Reflect) `elem` ys = [((x-1),1),((x+1),1)]
   | ((South,(x-2)),Reflect) `elem` ys && ((South,(x+2)),Reflect) `elem` ys = [((x-1),n),((x+1),n)]
   | ((North,(x-2)),Reflect) `elem` ys = [((x-1),1)]
   | ((South,(x-2)),Reflect) `elem` ys = [((x-1),n)]
   | ((North,(x+2)),Reflect) `elem` ys = [((x+1),1)]
   | ((South,(x+2)),Reflect) `elem` ys = [((x+1),n)]
   | otherwise = findNSDoubleRefl x (findAtomRep ys ++ findSameAtomRep (findAtomRep ys) ys ys)

-- these functions use information on already existing atoms to try and find the two atoms which caused the double reflection
findESDoubleRefl :: Int -> Atoms -> Atoms
findESDoubleRefl _ [] = []
findESDoubleRefl a ((x,y):xys)
   | (a+1) == y || (a-1) == y = [(x,(a+1)),(x,(a-1))]
   | otherwise = findESDoubleRefl a xys

findNSDoubleRefl :: Int -> Atoms -> Atoms
findNSDoubleRefl _ [] = []
findNSDoubleRefl a ((x,y):xys)
   | (a+1) == x || (a-1) == x = [((a+1),y),((a-1),y)]
   | otherwise = findNSDoubleRefl a xys



-- Challenge 3
-- Pretty Printing Alpha-Norms

data LamExpr =  LamApp LamExpr LamExpr  |  LamAbs Int LamExpr  |  LamVar Int 
                deriving (Eq, Show, Read)

prettyPrint :: LamExpr -> String
prettyPrint e = printLambda $ renameFullExpr $ chgLambda e

parens :: Bool -> String -> String
parens True s = "(" ++ s ++ ")"
parens False s = s

printLambda :: LamExpr -> String
printLambda (LamVar i) = "x" ++ show i
printLambda (LamAbs i b) = "\\x" ++ show i ++ " -> " ++ printLambda b
printLambda (LamApp (LamAbs i q) x) = "(" ++ printLambda (LamAbs i q) ++ ") " ++ printLambda x
printLambda (LamApp f x) = printLambda f ++ " " ++ printLambda x

chgLambda :: LamExpr -> LamExpr
chgLambda (LamVar i) = (LamVar i)
chgLambda (LamAbs i (LamAbs j (LamVar k)))
    | j == k && i /= j = (LamAbs j (LamAbs j (LamVar k)))
    | j /= k && i /= k = (LamAbs i (LamAbs i (LamVar k)))
chgLambda (LamAbs i e) = (LamAbs i (chgLambda e) )
chgLambda (LamApp (LamVar i) (LamAbs j (LamAbs k (LamVar l)))) = 
    if i == k && j == l 
        then (LamApp (LamVar (i+100)) (LamAbs j (LamAbs i (LamVar j)))) 
        else (LamApp (LamVar i) (LamAbs j (LamAbs k (LamVar l))))
chgLambda (LamApp a b) = (LamApp (chgLambda a) (chgLambda b))

renameFullExpr :: LamExpr -> LamExpr
renameFullExpr expr = renExprLoop (findExprInts expr) ([0..(length (findExprInts expr))]) expr

renExprLoop :: [Int] -> [Int] -> LamExpr -> LamExpr
renExprLoop [] _ expr = expr
renExprLoop (x:xs) (y:ys) expr = renExprLoop xs ys (rename1Expr expr x y)

rename1Expr :: LamExpr -> Int -> Int -> LamExpr
rename1Expr (LamVar x) i j
   | i == x = (LamVar j)
   | otherwise = (LamVar x)
rename1Expr (LamAbs x e) i j
   | i == x = (LamAbs j (rename1Expr e i j) )
   | otherwise = (LamAbs x (rename1Expr e i j) )
rename1Expr (LamApp e1 e2) i j = (LamApp (rename1Expr e1 i j) (rename1Expr e2 i j) )

findExprInts :: LamExpr -> [Int]
findExprInts expr = sort $ rmDuplicates $ freeVars expr

freeVars :: LamExpr -> [Int]
freeVars (LamVar s) = [s]
freeVars (LamAbs s t) = [s] ++ freeVars t
freeVars (LamApp t u) = freeVars t ++ freeVars u


-- Challenge 4 
-- Parsing Arithmetic Expressions

data ArithExpr = Add ArithExpr ArithExpr | Mul ArithExpr ArithExpr 
               | Section ArithExpr  | SecApp ArithExpr ArithExpr | ArithNum Int
    deriving (Show,Eq,Read) 



parseArith :: String -> Maybe ArithExpr
parseArith xs = parseNoSpaArith $ removeSpace $ xs

-- function that actually parses the input string of arithmetic expression. if the input string doesn't parse correctly or cannot be parsed, the function checks this by making sure that the second part of the parser is completely empty
parseNoSpaArith :: String -> Maybe ArithExpr
parseNoSpaArith xs
   | parse expr xs == [] = Nothing
   | snd (head (parse expr xs)) == [] = Just (fst (head (parse expr xs))) 
   | otherwise = Nothing

-- function to parse the (Mul e1 e2) part of the equation as it has highest precidence
expr :: Parser ArithExpr
expr = do t <- expr1
          char '*'
          e <- expr
          return (Mul t e)
       <|> expr1

-- function to parse the (Add e1 e2) part of the equation as it has second highest precidence
expr1 :: Parser ArithExpr
expr1 = do f <- expr2
           char '+'
           t <- expr1
           return (Add f t)
        <|> expr2

-- function that parses both the Integer Numbers, (SecApp (Section e1) e2) and parenthses "()" of the equation
expr2 :: Parser ArithExpr
expr2 = do x <- int
           return (ArithNum x)
        <|> do t <- section
               u <- expr2
               return (SecApp t u)
        <|> do char '('
               f <- expr
               char ')'
               return f

-- function to parse the (Section e1) part of the equation, as it has the lowest precidence
section :: Parser ArithExpr
section = do char '('
             char '+'
             f <- expr
             char ')'
             return (Section f)

-- this function removes any spaces or whitespace that may have been written in the string before it parses it, in order to make sure that the whitespace doesn't interfere with the parser
removeSpace :: String -> String
removeSpace str = filter (not . isSpace) str



-- Challenge 5
-- Church Encoding of arithmetic 

-- function that convers an arithmetic expression into a lambda calculus one. it does this by using pre-set definitions for both plus and multiply, and applies them to consecutive lambda expressions for the rest of the equation.
churchEnc :: ArithExpr -> LamExpr
churchEnc (Add e1 e2) = LamApp (LamApp ePlus (churchEnc e1)) (churchEnc e2)
  where ePlus = LamAbs 2 (LamAbs 3 ( LamAbs 0 ( LamAbs 1 ( LamApp ( LamApp (LamVar 2) (LamVar 0)) (LamApp (LamApp (LamVar 3) (LamVar 0)) (LamVar 1))))))

churchEnc (Mul e1 e2) = LamApp (LamApp eMul (churchEnc $ e1)) (churchEnc e2)
  where eMul  = LamAbs 2 (LamAbs 3 ( LamAbs 0 ( LamAbs 1 ( LamApp ( LamApp (LamVar 2) (LamApp (LamVar 3) (LamVar 0))) (LamVar 1)))))

churchEnc (SecApp e1 e2) = LamApp (LamApp ePlus (churchEnc e1)) (churchEnc e2)
  where ePlus = LamAbs 2 (LamAbs 3 ( LamAbs 0 ( LamAbs 1 ( LamApp ( LamApp (LamVar 2) (LamVar 0)) (LamApp (LamApp (LamVar 3) (LamVar 0)) (LamVar 1))))))

churchEnc (Section e1) = churchEnc e1

churchEnc (ArithNum n) = 
    LamAbs 0 (LamAbs 1 (getNum n (LamVar 0) (LamVar 1)))
  where getNum 0 e x = x
        getNum n e x = LamApp e (getNum (n-1) e x)


-- Challenge 6
-- Compare Innermost Reduction for Arithmetic and its Church Encoding

-- finds the leftmost innermost reduction of a lamda expression. if the lambda expression is already in its most reduced form, the function will return Nothing
innerRedn1 :: LamExpr -> Maybe LamExpr
innerRedn1 e
   | innerReduce e /= e = Just (innerReduce e)
   | innerReduce e == e && lambdaReduce e /= e = Just (lambdaReduce e)
   | innerReduce e == e && lambdaReduce e == e = Nothing

-- this function is the real innermost lambda calculus evaluator. this function will evaluate the innermost part of the lambda expression until it can't be evaluated any further (code for this function was inspired by the Lecture slides made by Julian Rathke)
innerReduce :: LamExpr -> LamExpr
innerReduce (LamApp (LamAbs x e1) e2) = substitute e1 x e2
innerReduce (LamApp e1 e2) = LamApp (innerReduce e1) e2
innerReduce (LamAbs x e) = LamAbs x (innerReduce e)
innerReduce (LamVar x) = LamVar x

-- this function is the real lambda calculus evaluator. this function will run when the complete innermost part of the lambda expression that has not already been evaluated (code for this function was inspired by the Lecture slides made by Julian Rathke)
lambdaReduce :: LamExpr -> LamExpr
lambdaReduce (LamApp (LamAbs x e1) e2) = substitute e1 x e2
lambdaReduce (LamApp e1 e2) = LamApp (lambdaReduce e1) (lambdaReduce e2)
lambdaReduce (LamAbs x e) = LamAbs x (lambdaReduce e)
lambdaReduce (LamVar x) = LamVar x

-- substitues the right part of the Lambda application into the left part of the Lambda application, and evaluates the result
substitute :: LamExpr -> Int -> LamExpr -> LamExpr
substitute (LamAbs y e1) x e2 = if x == y then (LamAbs y e1) else LamAbs y (substitute e1 x e2)
substitute (LamApp e1 e2) x t = LamApp (substitute e1 x t) (substitute e2 x t)
substitute (LamVar y) x e = if x == y then e else (LamVar y)

-- finds the leftmost innermost reduction of an arithmetic expression. if the arithmetic expression is already in its most reduced form, the function will return Nothing
innerArithRedn1 :: ArithExpr -> Maybe ArithExpr
innerArithRedn1 e
   | innerArithReduce e /= e = Just (innerArithReduce e)
   | innerArithReduce e == e && arithReduce e /= e = Just (arithReduce e)
   | innerArithReduce e == e && arithReduce e == e = Nothing

-- this function is the real innermost arithmetic expression evaluator. this function will evaluate the innermost part of the arithemtic expression until it can't be evaluated any further
innerArithReduce :: ArithExpr -> ArithExpr
innerArithReduce (ArithNum x) = (ArithNum x)
innerArithReduce (Add (ArithNum x) (ArithNum y)) = (ArithNum (x+y))
innerArithReduce (Mul (ArithNum x) (ArithNum y)) = (ArithNum (x*y))
innerArithReduce (SecApp (Section (ArithNum x)) (ArithNum y)) = (ArithNum (x+y))
innerArithReduce (Add e1 e2) = (Add (innerArithReduce e1) e2)
innerArithReduce (Mul e1 e2) = (Mul (innerArithReduce e1) e2)
innerArithReduce (SecApp (Section e1) e2) = (SecApp (Section (innerArithReduce e1)) e2)

-- this function is the real arithmetic expression evaluator. this function will run when the complete innermost part of the arithmetic expression that has not already been evaluated
arithReduce :: ArithExpr -> ArithExpr
arithReduce (ArithNum x) = (ArithNum x)
arithReduce (Add (ArithNum x) (ArithNum y)) = (ArithNum (x+y))
arithReduce (Mul (ArithNum x) (ArithNum y)) = (ArithNum (x*y))
arithReduce (SecApp (Section (ArithNum x)) (ArithNum y)) = (ArithNum (x+y))
arithReduce (Add e1 e2) = (Add e1 (arithReduce e2))
arithReduce (Mul e1 e2) = (Mul e1 (arithReduce e2))
arithReduce (SecApp (Section e1) e2) = (SecApp (Section e1 ) (arithReduce e2))

-- these three functions check to find any leftmost inner arithmetic expressions of the form (Mul e1 e2) and evaluate to find the value of e1. these functions do this in order to make up for the number of lost evaluations that occur when evaluating the lambda calculus equivalent of (Mul e1 e2) expressions using the compareArithLam function, because when the value of e3 is higher than the number 3, the lambda calculus loses 2 evaluations each time
mul3Check :: ArithExpr -> Int
mul3Check (Mul (ArithNum x) (ArithNum y)) = if x > 2 then mul3Int x else 0
mul3Check (Mul (ArithNum x) e2) = if x > 2 then mul3Int x + mul3Check e2 else 0
mul3Check (Add e1 e2) = (mul3Check e1) + (mul3Check e2)
mul3Check (Mul e1 e2) = (mul3Int (innerMul3Check e1)) + (mul3Check e2)
mul3Check (SecApp (Section e1) e2) = (mul3Check e1) + (mul3Check e2)
mul3Check e = 0

innerMul3Check :: ArithExpr -> Int
innerMul3Check (ArithNum x) = x
innerMul3Check (Add (ArithNum x) (ArithNum y)) = (x+y)
innerMul3Check (Mul (ArithNum x) (ArithNum y)) = if x > 2 then mul3Int x else 0
innerMul3Check (Mul (ArithNum x) e2) = if x > 2 then mul3Int x + mul3Check e2 else 0
innerMul3Check (SecApp (Section (ArithNum x)) (ArithNum y)) = (x+y)
innerMul3Check (Add e1 e2) = (innerMul3Check e1) + (innerMul3Check e2)
innerMul3Check (Mul e1 e2) = (innerMul3Check e1) + (innerMul3Check e2)
innerMul3Check (SecApp (Section e1) e2) = (innerMul3Check e1) + (innerMul3Check e2)

mul3Int :: Int -> Int
mul3Int 3 = 1
mul3Int x = 1 + mul3Int (x-1)

-- these two functions check to find any arithmetic expressions of the form (Add (Add e1 e2) e3) or (Add (SecApp (Section e1) e2) e3) and evaluate to find the number of times these appear. these functions do this in order to make up for the number of lost evaluations that occur when evaluating the lambda calculus equivalent of (Add (Add e1 e2) e3 or (Add (SecApp (Section e1) e2) e3) expressions using the compareArithLam function, because when the there are nested additions like the ones above, the compareArithLam function loses an evaluation each time
add3Check :: ArithExpr -> Int
add3Check (Add e1 e2) = (innerAdd3Check e1) + (innerAdd3Check e2)
add3Check (Mul e1 e2) = (add3Check e1) + (add3Check e2)
add3Check (SecApp (Section e1) e2) = (innerAdd3Check e1) + (innerAdd3Check e2)
add3Check e = 0

innerAdd3Check :: ArithExpr -> Int
innerAdd3Check (Add (ArithNum x) (ArithNum y)) = 1
innerAdd3Check (SecApp (Section (ArithNum x)) (ArithNum y)) = 1
innerAdd3Check (Add e1 e2) = (innerAdd3Check e1) + (innerAdd3Check e2)
innerAdd3Check (Mul e1 e2) = (innerAdd3Check e1) + (innerAdd3Check e2)
innerAdd3Check (SecApp (Section e1) e2) = (innerAdd3Check e1) + (innerAdd3Check e2)
innerAdd3Check e = 0

-- this function counts the number of times the lambda calculus evaluator evaluates an expression and converts it into an integer for the compareArithLam function
numInnerRedn1 :: Maybe LamExpr -> Int -> Int
numInnerRedn1 (Nothing) x = x-1
numInnerRedn1 (Just e) x  = numInnerRedn1 (innerRedn1 e) (x+1)

-- this function counts the number of times the arithmetic expression evaluator evaluates an expression and converts it into an integer for the compareArithLam function
numArithRedn1 :: Maybe ArithExpr -> Int -> Int
numArithRedn1 (Nothing) x = x-1
numArithRedn1 (Just e) x  = numArithRedn1 (innerArithRedn1 e) (x+1)

-- this function takes the values from the previous two functions and outputs them as a pair of Int, which shows the number of evaluations each expression evaluator has gone through. Because my lambda calculus evaluator skips certain evaluations, in order to make uo for the skipped evaluations, I use two extra functions to add on the extra skipped values at the end
compareArithLam :: ArithExpr -> (Int,Int)
compareArithLam (ArithNum e) = (0,0)
compareArithLam e = ((numArithRedn1 (Just e) 0) , (numInnerRedn1 (Just $ churchEnc e) 0) + mul3Check e + add3Check e)


-- Extra stuff to help


-- this function takes a list as an input and removes any duplicate items in that list. this function was used for certain functions related to the answers of challenge exercises 1 and 2
-- taken from: https://stackoverflow.com/questions/16108714/removing-duplicates-from-a-list-in-haskell-without-elem
rmDuplicates :: Eq a => [a] -> [a]
rmDuplicates [] = []
rmDuplicates (a:as)
   | a `elem` as   = rmDuplicates as
   | otherwise     = a : rmDuplicates as

