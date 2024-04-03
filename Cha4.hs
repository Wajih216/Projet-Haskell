module Cha4 (challenge4) where
import CPL

-- La porte 1 dit la vérité quand il y a une peluche et ment quand c'est un tigre
-- L'affiche de la porte 1 dit : "Choisis n'importe quelle cellule, ça n'a pas d'importance !"
door1 :: Formula
door1 = Eqv (And (Var "p1") (Not (Var "t1"))) (Or (And (Var "p1") (Var "p2")) (And (Var "t1") (Var "t2")))

-- La porte 2 dit la vérité quand il y a un tigre et ment quand il y a une peluche
-- L'affiche de la porte 2 dit : "Il y a une peluche dans l'autre cellule"
door2 :: Formula
door2 = Eqv (And (Var "t2") (Not (Var "p2"))) (Var "p1")

-- Contrainte : Une cellule ne peut pas contenir à la fois une peluche et un tigre
constraints :: Formula
constraints = And (Eqv (Var "p1") (Not(Var "t1"))) (Eqv (Var "p2") (Not(Var "t2"))) 

-- Règlement : Dans la première épreuve au moins, l'une des portes dit la vérité et l'autre ment
reglement4 :: Formula
reglement4 = And door1 door2


-- Challenge 1 : Conjuguaison de la contrainte et du règlement
challenge4 :: Formula
challenge4 = And (reglement4) (constraints)


