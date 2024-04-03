module Cha5 (challenge5) where
import CPL

-- Formules existantes
door1 :: Formula
door1 = Eqv (And (Var "p1") (Not (Var "t1"))) (Or (And (Var "p1") (Var "t2")) (And (Var "t1") (Var "p2")))

door2 :: Formula
door2 = Eqv (And (Var "t2") (Not (Var "p2"))) (Var "p1")

-- Contrainte : Une cellule ne peut pas contenir à la fois une peluche et un tigre
constraints :: Formula
constraints = And (Eqv (Var "p1") (Not(Var "t1"))) (Eqv (Var "p2") (Not(Var "t2"))) 

-- Règlement : Dans la première épreuve au moins, l'une des portes dit la vérité et l'autre ment
reglement5 :: Formula
reglement5 = And door1 door2

-- Challenge 1 : Conjuguaison de la contrainte et du règlement
challenge5 :: Formula
challenge5 = And (reglement5) (constraints)


