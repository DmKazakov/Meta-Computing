module Main where

import           Control.Applicative (liftA2)
import           Data.List           (find, intersect)
import           Lib

main :: IO ()
main = someFunc

type Name = String

type Def = (Name, Exp)

data Prg =
    Prg [Def] Exp

data Exp
    = Var Name
    | Fun Name
    | Const [Exp]
    | App Exp Exp
    | Lmb Name Exp
    | Let Name Exp Exp
    | Case Exp [([Name], Exp)]

eval :: Prg -> Exp
eval (Prg fs exp) = eval' exp
  where
    isVal :: Exp -> Bool
    isVal (Var _)    = True
    isVal (Const es) = and $ fmap isVal es
    isVal _          = False
    isMatch :: Exp -> ([Name], Exp) -> Bool
    isMatch (Const es) (ns, e) = length es == length ns
    isMatch _ _                = False
    subst :: Name -> Exp -> Exp -> Exp
    subst n e (Lmb n' e')
        | n' == n = Lmb n' e'
        | otherwise = Lmb n' $ subst n e e'
    subst n e (Let n' e' b)
        | n' == n = Let n' (subst n e e') b
        | otherwise = Let n' (subst n e e') (subst n e b)
    subst n e (Case e' ps) = Case (subst n e e') $ fmap substPat ps
      where
        substPat :: ([Name], Exp) -> ([Name], Exp)
        substPat p@(ns, pe)
            | n `elem` ns = p
            | otherwise = st pe [] ns
          where
            st :: Exp -> [Name] -> [Name] -> [Name] -> ([Name], Exp)
            st exp cns (on:ons) = st exp (cns ++ [on]) ons
            st exp cns [] fv = (cns, subst n e exp)
    subst n e v@(Var n')
        | n == n' = e
        | otherwise = v
    subst n e (Fun f) =
        case lookup f fs of
            Just e' -> subst n e e'
    subst n e (App e1 e2) = App (subst n e e1) (subst n e e2)
    subst n e (Const es) = Const $ fmap (subst n e) es
    substPat :: [Name] -> [Exp] -> Exp -> Exp
    substPat (n:ns) (e:es) exp = substPat ns es $ subst n e exp
    substPat [] _ exp          = exp
    eval' :: Exp -> Exp
    eval' v@(Var _) = v
    eval' l@(Lmb _ _) = l
    eval' (Fun f) =
        case lookup f fs of
            Just e -> eval' e
    eval' (Const es) = Const $ fmap eval' es
    eval' a@(App (Lmb n b) e) = eval' $ subst n e b
    eval' a@(App e1 e2)
        | isVal e1 = a
        | otherwise = eval' $ App (eval' e1) e2
    eval' (Let n e b) = eval' $ subst n e $ eval' b
    eval' (Case e ps)
        | isVal e
        , Const es <- e
        , Just (ns, e1) <- find (isMatch e) ps = eval' $ substPat ns es e1
        | not $ isVal e = eval' $ Case (eval' e) ps

newtype Subst =
    Subst [(Name, Exp)]
