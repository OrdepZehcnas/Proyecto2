{-|
Module      : PureISWIM
Description : An implementation of the beta reduction of the Pure ISWIM Calculus
Copyright   : (c) Fernando A. Galicia Mendoza, 2019
License     : GPL-3
Maintainer  : fernandogamen@ciencias.unam.mx
Stability   : experimental-education
Portability : POSIX

This script contains the functions to calculate the reduction of abstractions
of the Pure ISWIM Calculus.
It's important to observe that the reduction strategy is a simulation of the
non-determinism of the following rule:

t1 -> t1' t2 -> t2'
---------------------
t1 t2 -> t1' t2'

We analize which of the terms is in normal form, e.g. if the t1 is in normal
form then we reduce the other term. In other words, we substitute the previous rule
with the two following:

(t1 is normal) t2 -> t2'
---------------------
t1 t2 -> t1 t2'

(t2 is normal) t1 -> t1'
---------------------
t1 t2 -> t1' t2
-}
module ISWIM where

  import Data.List
  import Data.Char

  -- | Type that represents the set of possible variable names.
  type Name = String

  data ISWIM = V Name -- ^ Constructor for the variables.
              | App ISWIM ISWIM -- ^ Constructor for the applications.
              | L Name ISWIM -- ^ Constructor for the abstractions.
              | LetFun Name Name ISWIM ISWIM -- ^ Constructor for the name functions.
              | N Int 
              | B Bool
              | Suc ISWIM
              | Pred ISWIM
              | Plus ISWIM ISWIM
              | Prod ISWIM ISWIM
              | Neg ISWIM
              | Conj ISWIM ISWIM
              | Disy ISWIM ISWIM
              | Gt ISWIM ISWIM
              | Lt ISWIM ISWIM
              | Equi ISWIM ISWIM
              | Ift ISWIM ISWIM ISWIM
              | LetE Name ISWIM ISWIM

  -- Instance for a pretty/decent print.
  instance Show ISWIM where
    show l = case l of
      V x -> x
      App t s -> "("++show t++" <+> "++show s++")"
      L x t -> "(lam "++x++" -> "++show t++")"
      LetE x t1 t2 -> "let \n"++x++":="++show t1++"\nin\n"++show t2++"\nend"
      LetFun f x t1 t2 -> "let fun \n"++f++" "++x++" "++" => "++show t1++"\nin\n"++show t2++"\nend"
      N n -> "num["++show n++"]"
      B b -> "bool["++show b++"]"
      Suc e-> "suc("++show e++")"
      Pred e-> "pred("++show e++")"
      Plus e1 e2-> "plus("++show e1++","++show e2++")"
      Prod e1 e2-> "prod("++show e1++","++show e2++")"
      Neg e-> "neg("++show e++")"
      Conj e1 e2-> "and("++show e1++","++show e2++")"
      Disy e1 e2-> "or("++show e1++","++show e2++")"
      Gt e1 e2-> "gt("++show e1++","++show e2++")"
      Lt e1 e2-> "lt("++show e1++","++show e2++")"
      Equi e1 e2-> "eq("++show e1++","++show e2++")"
      Ift e1 e2 e3-> "if("++show e1++","++show e2++","++show e3++")"

  -- | Type that represents the substitution.
  type Subst = (Name,ISWIM)

  -- | The desugar function unfold the definitions of let and letfun
  desugar :: ISWIM -> ISWIM
  desugar i = error "Funcion no implementada"

  -- | The fv function takes an abstraction and returns their free variables.
  fv :: ISWIM -> [Name]
  fv i = error "Funcion no implementada"

  {-|
  The newId function creates a new variable with the following conditions:
  1. If at the end of the variable is not a number then the function 
  add the number 0 at the end of the variable.
  2. If at the end of the variable is a number then the function
  replace the original number with its sucessor.
  -} 
  newId :: Name -> Name
  newId x = nuevoId x ""


  {-|
  The nuevoId function tries to split strings of the form "vn" returning
  the pair (v,n) where "v" could be any string but "n" is a string with only numbers.
  If the string doesn't end with a number then "n" will be equal to the empty string.
  -}
  nuevoId :: Name -> Name -> Name
  nuevoId [] acc = acc ++ "0"
  nuevoId n@(x:xs) acc = case isDigit x of
    True -> acc ++ show ((read n :: Int) + 1) 
    False -> nuevoId xs (acc++[x])

  -- | The alpha function generates the alpha-equivalence of a ISWIM term.
  alpha :: ISWIM -> ISWIM
  alpha i = error "Funcion no implementada"

  -- | The substitution function applies the substitution given as a parameter to a ISWIM term.
  substitution :: ISWIM -> Subst -> ISWIM
  substitution i s = error "Funcion no implementada"

  -- | The eval function is an implementation of the evaluation.
  eval1 :: ISWIM -> ISWIM
  eval1 _ = error "Execution Error: Locked state."

  isValue :: ISWIM -> Bool
  isValue (N _) = True
  isValue (B _) = True
  isValue _ = False

  {-| 
  The betas function is a implementation of the reflexive-transitive closure of the beta
  reduction. Before the application of the beta reduction, this function test if the ISWIM
  term is in normal form.
  -}
  evals' :: ISWIM -> ISWIM
  evals' t = error "Funcion no implementada"

  evals :: ISWIM -> ISWIM
  evals t = evals' (desugar t)  