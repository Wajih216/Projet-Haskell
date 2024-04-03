module Cha2 (challenge2) where
import CPL

-- Formules existantes
door1 :: Formula
door1 = Or (Var "p1") (Var "p2")

door2 :: Formula
door2 = Var "t1"

-- Contrainte : Une cellule ne peut pas contenir à la fois une peluche et un tigre
constraints :: Formula
constraints = And (Eqv (Var "p1") (Not(Var "t1"))) (Eqv (Var "p2") (Not(Var "t2"))) 

-- Règlement : Dans la première épreuve au moins, l'une des portes dit la vérité et l'autre ment
reglement2 :: Formula
reglement2 = Or (And door1 door2) (And (Not door1) (Not door2)) 

-- Challenge 1 : Conjuguaison de la contrainte et du règlement
challenge2 :: Formula
challenge2 = And (reglement2) (constraints)