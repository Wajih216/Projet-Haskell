module Cha1 (challenge1) where
import CPL

-- Formules existantes
door1 :: Formula
door1 = And (Var "p1") (Var "t2")

door2 :: Formula
door2 = Or (And (Var "p1") (Var "t2")) (And (Var "t1") (Var "p2"))

-- Contrainte : Une cellule ne peut pas contenir à la fois une peluche et un tigre
constraints :: Formula
constraints = And (Eqv (Var "p1") (Not(Var "t1"))) (Eqv (Var "p2") (Not(Var "t2"))) 

-- Règlement : Dans la première épreuve au moins, l'une des portes dit la vérité et l'autre ment
reglement1 :: Formula
reglement1 = Or (And (Not door1) door2) (And door1 (Not door2)) 

-- Challenge 1 : Conjuguaison de la contrainte et du règlement
challenge1 :: Formula
challenge1 = And (reglement1) (constraints)


