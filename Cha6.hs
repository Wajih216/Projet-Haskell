module Cha6 (challenge6) where
import CPL

-- Formules existantes
door1 :: Formula
door1 = Var "t1"

door2 :: Formula
door2 = Var "p2"

door3 :: Formula
door3 = Var "t2"

-- Contrainte : Une cellule ne peut pas contenir à la fois une peluche et un tigre
onlyOneTeddy :: Formula
onlyOneTeddy = Or (And (Var "p1") (And (Not (Var "p2")) (Not (Var "p3"))))
                  (Or (And (Not (Var "p1")) (And (Var "p2") (Not (Var "p3"))))
                      (And (Not (Var "p1")) (And (Not (Var "p2")) (Var "p3"))))

-- Contrainte : Une cellule ne peut pas contenir à la fois une peluche et un tigre
constraints :: Formula
constraints = And (And (And (Eqv (Var "p1") (Not(Var "t1"))) (Eqv (Var "p2") (Not(Var "t2")))) (Eqv (Var "p3") (Not(Var "t3")))) onlyOneTeddy

-- Règlement : Dans la sixième épreuve au moins, l'une des portes dit la vérité et les autres mentent
reglement6 :: Formula
reglement6 = Or (And door1 (And (Not door2) (Not door3))) 
              (Or (And (Not door1) (And door2 (Not door3))) 
                  (And (Not door1) (And (Not door2) door3)))

-- Challenge 1 : Conjuguaison de la contrainte et du règlement
challenge6 :: Formula
challenge6 = And (reglement6) (constraints)


