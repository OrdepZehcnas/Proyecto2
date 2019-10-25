{-|
Module      : MiniHs
Description : An implementation of the evaluation of typed Mini Haskell programs.
Copyright   : (c) Fernando Abigail Galicia-Mendoza, 2019
License     : GPL-3
Maintainer  : fernandogamen@ciencias.unam.mx
Stability   : experimental-education
Portability : POSIX

This script contains the functions to calculate the evaluation of typed Mini Haskell
Programs.
-}
module MiniHs where

  import Data.List
  import Data.Char

  -- | Type that represents the set of possible variable names.
  type Name = String

  -- | A 'MiniHs' is a implementation of the abstract syntax of typed Mini Haskell programs.
  data MiniHs = V Name -- ^ Constructor for the typed variables.
              | N Int -- ^ Constructor for the typed numbers.
              | B Bool -- ^ Constructor for the typed booleans.
              | Suc MiniHs -- ^ Constructor for the typed successor operator.
              | Pred MiniHs -- ^ Constructor for the typed predecessor operator.
              | Plus MiniHs MiniHs -- ^ Constructor for the typed plus operator.
              | Prod MiniHs MiniHs -- ^ Constructor for the typed product operator.
              | Neg MiniHs -- ^ Constructor for the typed negation operator.
              | Conj MiniHs MiniHs -- ^ Constructor for the typed conjunction operator.
              | Disy MiniHs MiniHs -- ^ Constructor for the typed disjunction operator.
              | Gt MiniHs MiniHs -- ^ Constructor for the typed greater than operator.
              | Lt MiniHs MiniHs -- ^ Constructor for the typed lower than operator.
              | Equi MiniHs MiniHs -- ^ Constructor for the typed equality operator.
              | Ift MiniHs MiniHs MiniHs -- ^ Constructor for the typed if-else conditional.
              | L Name Type MiniHs -- ^ Constructor for the typed abstraction.
              | Fix Name Type MiniHs -- ^ Constructor for the typed fix operator.
              | App MiniHs MiniHs -- ^ Constructor for the typed application.

  -- | A 'MiniHsU' is a implementation of the abstract syntax of untyped Mini Haskell programs.
  data MiniHsU = VU Name -- ^ Constructor for the untyped variables.
               | NU Int -- ^ Constructor for untyped numbers.
               | BU Bool -- ^ Constructor for untyped booleans.
               | SucU MiniHsU -- ^ Constructor for untyped successor operator.
               | PredU MiniHsU -- ^ Constructor for untyped predecessor operator.
               | PlusU MiniHsU MiniHsU -- ^ Constructor for untyped plus operator.
               | ProdU MiniHsU MiniHsU -- ^ Constructor for untyped product operator.
               | NegU MiniHsU -- ^ Constructor for untyped negation operator.
               | ConjU MiniHsU MiniHsU -- ^ Constructor for untyped conjunction operator.
               | DisyU MiniHsU MiniHsU -- ^ Constructor for untyped disjunction operator.
               | GtU MiniHsU MiniHsU -- ^ Constructor for untyped greater than operator.
               | LtU MiniHsU MiniHsU -- ^ Constructor for untyped lower than operator.
               | EquiU MiniHsU MiniHsU -- ^ Constructor for untyped equality operator.
               | IftU MiniHsU MiniHsU MiniHsU -- ^ Constructor for untyped if-else operator.
               | LU Name MiniHsU -- ^ Constructor for untyped abstraction.
               | FixU Name MiniHsU -- ^ Constructor for untyped fix operator.
               | AppU MiniHsU MiniHsU -- ^ Constructor for untyped application operator.

  -- | A 'Type' is a implementation of the abstract syntax of types.
  data Type = NAT | BOOL | T Name | Type :-> Type deriving(Show,Eq)

  -- | A 'Decl' is a pair that represents the declaration of a variable x
  -- has a type T.
  type Decl = (Name,Type)

  -- | A 'TypeCtx' is the list of the declarations of variables.
  type TypeCtx = [Decl]

  instance Show MiniHs where
    show l = case l of
      V x -> x
      N n -> "num["++show n++"]"
      B b -> "bool["++show b++"]"
      Suc e-> "suc("++show e++")"
      Pred e-> "pred("++show e++")"
      Plus e1 e2-> "("++show e1++"+"++show e2++")"
      Prod e1 e2-> "("++show e1++"*"++show e2++")"
      Neg e-> "!("++show e++")"
      Conj e1 e2-> "("++show e1++"&&"++show e2++")"
      Disy e1 e2-> "("++show e1++"||"++show e2++")"
      Gt e1 e2-> "("++show e1++">"++show e2++")"
      Lt e1 e2-> "("++show e1++"<"++show e2++")"
      Equi e1 e2-> "("++show e1++"=="++show e2++")"
      Ift e1 e2 e3-> "if "++show e1++" then "++show e2++" else "++show e3
      L x ty1 t -> "(lam ("++x++" : "++show ty1++")"++" => "++show t++")"
      Fix f ty t -> "(fix "++f++" : "++show ty++" => "++show t++")"
      App t s -> "("++show t++" <+> "++show s++")"

  instance Show MiniHsU where
    show l = case l of
      VU x -> x
      NU n -> "num["++show n++"]"
      BU b -> "bool["++show b++"]"
      SucU e-> "suc("++show e++")"
      PredU e-> "pred("++show e++")"
      PlusU e1 e2-> "("++show e1++"+"++show e2++")"
      ProdU e1 e2-> "("++show e1++"*"++show e2++")"
      NegU e-> "!("++show e++")"
      ConjU e1 e2-> "("++show e1++"&&"++show e2++")"
      DisyU e1 e2-> "("++show e1++"||"++show e2++")"
      GtU e1 e2-> "("++show e1++">"++show e2++")"
      LtU e1 e2-> "("++show e1++"<"++show e2++")"
      EquiU e1 e2-> "("++show e1++"=="++show e2++")"
      IftU e1 e2 e3-> "if "++show e1++" then "++show e2++" else "++show e3
      LU x t -> "(lam "++x++" => "++show t++")"
      FixU f t -> "(fix "++f++" => "++show t++")"
      AppU t s -> "("++show t++" <+> "++show s++")"

  -- | A 'Subst' represents a substitution.
  type Subst = (Name,MiniHs)

  -- | The 'fv' function takes a typed Mini Haskell program and returns their free variables.
  fv :: MiniHs -> [Name]
  fv e = error "Funcion no implementada" 

  {-|
  The 'newId' function creates a new variable with the following conditions:
  1. If at the end of the variable is not a number then the function 
  add the number 0 at the end of the variable.
  2. If at the end of the variable is a number then the function
  replace the original number with its sucessor.
  -} 
  newId :: Name -> Name
  newId x = nuevoId x ""

  {-|
  The 'nuevoId' function tries to split strings of the form "vn" returning
  the pair (v,n) where "v" could be any string but "n" is a string with only numbers.
  If the string doesn't end with a number then "n" will be equal to the empty string.
  -}
  nuevoId :: Name -> Name -> Name
  nuevoId [] acc = acc ++ "0"
  nuevoId n@(x:xs) acc = case isDigit x of
    True -> acc ++ show ((read n :: Int) + 1) 
    False -> nuevoId xs (acc++[x])

  -- | The 'alpha' function generates the alpha-equivalence of a typed Mini Haskell program.
  alpha :: MiniHs -> MiniHs
  alpha e = error "Funcion no implementada" 

  -- | The 'substitution' function applies the substitution given as 
  -- a parameter to a typed Mini Haskell program.
  substitution :: MiniHs -> Subst -> MiniHs
  substitution _ _ = error "Funcion no implementada" 

  -- | The 'eval' function is an implementation of the evaluation for typed Mini Haskell
  -- programs.
  eval1 :: MiniHs -> MiniHs
  eval1 _ = error "Execution Error: Locked state."

  -- | The 'isValue' is the predicate that determines if a typed Mini Haskell
  -- program is a value. (De nada :) ) 
  isValue :: MiniHs -> Bool
  isValue (N _) = True
  isValue (B _) = True
  isValue (L _ _ _) = True
  isValue _ = False

  {-| 
  The 'evals' function is the implementation of the relexive-transitive closure
  of the evaluation relation.
  -}
  evals' :: MiniHs -> MiniHs
  evals' t = error "Funcion no implementada" 