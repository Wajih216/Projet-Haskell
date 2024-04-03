module CPL (
    Formula (..),
    World,
    genAllWorlds,
    testGenAllWorlds,
    sat,
    testSat,
    extract,
    testExtract,
    findWorlds,
    testFindWorlds,
    testAll
) where


import Data.List (nub)
import Data.List (sort)

-- Définition des formules
data Formula = T
             | F
             | Var String
             | Not Formula
             | And Formula Formula
             | Or Formula Formula
             | Imp Formula Formula
             | Eqv Formula Formula
             deriving (Show)


type World = [String]

w0 :: World
w0 = ["p1", "p2"]

genAllWorlds :: World -> [World]
genAllWorlds [] = [[]] -- Cas de base : liste vide de variables propositionnelles
genAllWorlds (x:xs) = let rest = genAllWorlds xs
                    in rest ++ map (x:) rest

testGenAllWorlds :: [Bool]
testGenAllWorlds = [length (genAllWorlds w0) == 4]

sat :: World -> Formula -> Bool
sat _ T = True
sat _ F = False
sat w (Var p) = p `elem` w
sat w (Not phi) = not (sat w phi)
sat w (And phi psi) = sat w phi && sat w psi
sat w (Or phi psi) = sat w phi || sat w psi
sat w (Imp phi psi) = not (sat w phi) || sat w psi
sat w (Eqv phi psi) = sat w phi == sat w psi

testSat :: [Bool]
testSat = [sat w0 (And (Var "p1") (Var "p2"))]

extract :: Formula -> World
extract formula = nub (extract' formula [])
    where
        extract' (Var p) list = p : list
        extract' (Not phi) list = extract' phi list
        extract' (And phi psi) list = extract' phi (extract' psi list)
        extract' (Or phi psi) list = extract' phi (extract' psi list)
        extract' (Imp phi psi) list = extract' phi (extract' psi list)
        extract' (Eqv phi psi) list = extract' phi (extract' psi list)

testExtract :: [Bool]
testExtract = [sort (extract (And (Var "p1") (Var "t2"))) == sort ["p1", "t2"]]

findWorlds :: Formula -> [World]
findWorlds formule = [ world | world <- genAllWorlds (extract formule), sat world formule ]

testFindWorlds :: [Bool]
testFindWorlds = [all (\world -> sat world (And (Var "p1") (Var "t2"))) (findWorlds (And (Var "p1") (Var "t2")))]

testResults = concat [testGenAllWorlds, testSat, testExtract, testFindWorlds]

testAll :: [Bool] -> String
testAll results
    | all (==True) results = "Success"
    | otherwise = "Failure"




