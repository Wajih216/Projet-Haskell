module Cha3 (challenge3) where
import CPL

-- Formules existantes
door1 :: Formula
door1 = Or (Var "t1") (Var "p2")

door2 :: Formula
door2 = Var "p1"

-- Contrainte : Une cellule ne peut pas contenir à la fois une peluche et un tigre
constraints :: Formula
constraints = And (Eqv (Var "p1") (Not(Var "t1"))) (Eqv (Var "p2") (Not(Var "t2"))) 

-- Règlement : Dans la première épreuve au moins, l'une des portes dit la vérité et l'autre ment
reglement3 :: Formula
reglement3 = Or (And door1 door2) (And (Not door1) (Not door2)) 

-- Challenge 1 : Conjuguaison de la contrainte et du règlement
challenge3 :: Formula
challenge3 = And (reglement3) (constraints)


