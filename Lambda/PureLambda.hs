{-|
Module      : PureLambda
Description : An implementation of the beta reduction of the Pure Lambda Calculus
Copyright   : (c) Fernando Abigail Galicia-Mendoza, 2019
License     : GPL-3
Maintainer  : fernandogamen@ciencias.unam.mx
Stability   : experimental-education
Portability : POSIX

This script contains the functions to calculate the reduction of abstractions
of the Pure Lambda Calculus.
It's important to observe that the reduction strategy is a simulation of the
non-deterministic rules:

t1 -> t1'
---------------------
t1 t2 -> t1' t2'

t2 -> t2'
---------------------
t1 t2 -> t1' t2'

We analyze which of the terms is in normal form, e.g. if the t1 is in normal
form then we reduce the other term. In other words, we substitute the previous rule
with the two following:

(t1 is normal) t2 -> t2'
---------------------
t1 t2 -> t1 t2'

(t2 is normal) t1 -> t1'
---------------------
t1 t2 -> t1' t2
-}
module PureLambda where

  import Data.List
  import Data.Char

  -- | Type that represents the set of possible variable names.
  type Name = String

  -- A 'Lambda' is a representation of a pure lambda term.
  data Lambda = V Name -- ^ Constructor for the variables.
              | App Lambda Lambda -- ^ Constructor for the applications.
              | L Name Lambda -- ^ Constructor for the abstractions.
              | Let Name Lambda Lambda -- ^ Constructor for the let.
              | Fun Name Lambda -- ^ (NEW) Syntax sugar for lambda
              | LetFun Name Name Lambda Lambda -- ^ Constructor for the name functions.

  instance Show Lambda where
    show l = case l of
      V x -> x
      App t s -> "("++show t++" <+> "++show s++")"
      L x t -> "(lam ("++x++") -> "++show t++")"
      Fun x t -> "(fun ("++x++") => "++show t++")"
      Let x t1 t2 -> "let \n"++x++":="++show t1++"\nin\n"++show t2++"\nend"
      LetFun f x t1 t2 -> "let fun \n"++f++" "++x++" "++" => "++show t1++"\nin\n"++show t2++"\nend"

  -- | A 'Subst' represents the substitution.
  type Subst = (Name,Lambda)

  -- | The 'desugar' function unfold the definitions of fun, let and letfun expressions.
  desugar :: Lambda -> Lambda
  desugar l = error "Funcion no implementada" 

  -- | The 'fv' function takes an abstraction and returns their free variables.
  fv :: Lambda -> [Name]
  fv e = error "Funcion no implementada" 


  {-|
  The newId function creates a new variable with the following conditions:
  1. If at the end of the variable is not a number then the function 
  add the number 0 at the end of the variable.
  2. If at the end of the variable is a number then the function
  replace the original number with its sucessor.
  -} 
  newId :: Name -> Name
  newId x = splitName x ""

  {-|
  The splitName function tries to split strings of the form "vn" returning
  the the variable "v(n+1)". E.g. splitName "x0" "" then returns "x1".
  -}
  splitName :: Name -> Name -> Name
  splitName [] acc = acc ++ "0"
  splitName n@(x:xs) acc = case isDigit x of
    True -> acc ++ show ((read n :: Int) + 1) 
    False -> splitName xs (acc++[x])

  -- | The 'alpha' function generates the alpha-equivalence of a lambda term.
  alpha :: Lambda -> Lambda
  alpha l = error "Funcion no implementada"

  -- | The 'substitution' function applies the substitution given as a parameter to a lambda term.
  substitution :: Lambda -> Subst -> Lambda
  substitution l s = error "Funcion no implementada"

  -- | The 'beta' function is an implementation of the beta reduction.
  beta :: Lambda -> Lambda
  beta l = error "Funcion no implementada"

  -- | The 'normal' function is the predicate that is True iff a lambda term is in normal form.
  normal :: Lambda -> Bool
  normal l = error "Funcion no implementada" 

  {-| 
  The 'betas' function is a implementation of the reflexive-transitive closure of the beta
  reduction. Before the application of the beta reduction, this function test if the lambda
  term is in normal form.
  -}
  betas :: Lambda -> Lambda
  betas t = error "Funcion no implementada"