{-|
Module      : W
Description : An implementation of the W function.
Copyright   : (c) Fernando Abigail Galicia-Mendoza, 2019
License     : GPL-3
Maintainer  : fernandogamen@ciencias.unam.mx
Stability   : experimental-education
Portability : POSIX

This script implements the W function.
-}
module W where

  import MiniHs
  import Unifier
  import Data.List

  -- |The 'context' is represented as a set of different declarations of variables with their respective types
  type Ctx = [(Name, Type)]

  -- |A 'Judgment' is an Assertion (G, e, T) that represents that in the context G, e has type T
  data Judgment = Assertion (Ctx, MiniHs, Type)

  instance Show Judgment where
    show (Assertion (ctx, e, t)) = "Context: " ++ show ctx ++ " \n\n" ++ show e ++ " : " ++ show t

  -- | The 'w' function takes an untyped Mini Haskell program e and returns a judgment of
  -- the form (G,ea,T) where G is the context, ea is the type version of e and T is the
  -- type of the program.
  w :: MiniHsU -> Judgment
  w e = error "Funcion no implementada" 

  -- | The 'w_aux' function takes a context and an untyped Mini Haskell program, and
  -- apply the w function.
  w_aux :: Ctx -> MiniHsU -> Judgment 
  w_aux c e = error "funcion no implementada"

  -- | The 'hasDecl' function takes a variable expression x and a context G
  -- and in the case that G has a declaration of x, returns the type [t] of x 
  -- If the declaration does not exists, the function returns [].
  hasDecl :: Name -> Ctx -> [Type]
  hasDecl _ _ = error "funcion no implementada" 

  -- | The 'typesG' function returns all the variables of type that appears 
  -- in a given context.
  typesG :: Ctx -> [Name]
  typesG [] = error "funcion no implementada"

  -- | The 'types' function returns all the variables of type that appears
  -- in a type.
  types :: Type -> [Name]
  types (T x) = error "funcion no implementada"

  -- | The 'types' function returns all the variables of type that appears
  -- in a typed Mini Haskell program.
  typesE :: MiniHs -> [Name]
  typesE e = error "funcion no implementada"
  
  -- | The 'getMu' returns (if it is possible) the umg of a list
  -- of substitutions.
  getMu :: [Substitution] -> Substitution
  getMu _ = error "funcion no implementada"

  -- | The 'appUmgCtx' returns the application of a umg over a context.
  appUmgCtx :: Ctx -> Substitution -> Ctx
  appUmgCtx _ _ = error "funcion no implementada"

  -- | The 'appUmgExp' returns the application of a umg over a typed Mini Haskell program.
  appUmgExp :: MiniHs -> Substitution -> MiniHs
  appUmgExp _ _ = error "funcion no implementada"

  -- | The 'appUmgType' returns the application of a umg over a type.
  appUmgType :: Type -> Substitution -> Type
  appUmgType t s = error "funcion no implementada"

  -- | The 'newVType' function returns a new variable type that is not contained in the given set. (Regalito :) )
  newVType :: [Name] -> Name
  newVType [] = "X"
  newVType (v:vs) = let v' = newId v in 
                     if v' `elem` vs then newVType (v':vs) else v'

  -- | The 'evals' function compute the judgment of a untyped Mini Haskell program
  -- and returns the evaluation of the typed version.
  evals :: MiniHsU -> MiniHs
  evals e = error "funcion no implementada"