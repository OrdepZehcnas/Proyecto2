{-|
Module      : Unifier
Description : Provides the implementation of Martelli Montanari Unification Algorithm 
              for Types.
Copyright   : (c) Fernando Abigail Galicia-Mendoza, 2019
License     : GPL-3
Maintainer  : pablog@ciencias.unam.mx
              fernandogamen@ciencias.unam.mx
Stability   : experimental-education
Portability : POSIX
-}
module Unifier where

  import MiniHs

  -- | A 'Substitution' is represented as a set of different pairs (Identifier, Type)
  type Substitution = [(Name, Type)]

  -- |The 'subst' function apply a substitution on a type.
  subst :: Type -> Substitution -> Type
  subst t s
    = case t of
      NAT -> NAT
      BOOL -> BOOL
      T x
        -> case s of
          [] -> T x 
          ((y, t') : ss) -> if x == y then t' else subst t ss
      t1 :-> t2 -> subst t1 s :-> subst t2 s

  -- |The 'o' function defines the composition of substitutions.
  o :: Substitution -> Substitution -> Substitution
  o s1 s2 = simplSubst [(x, subst t s2) | (x, t) <- s1] ++ [(y, t) | (y, t) <- s2, notElem y [x | (x, t) <- s1]]

  -- |The 'simplSubst' function remove the repeated pairs in a substitution.
  simplSubst :: Substitution -> Substitution
  simplSubst [] = []
  simplSubst ((x, t) : ss)
    = case t of
      T y -> if x == y then simplSubst ss else (x, t) : simplSubst ss
      _ -> (x, t) : simplSubst ss

  -- |The 'mu' function returns the most general unifier of a set of type equations.
  mu :: [(Type, Type)] -> [Substitution]
  mu [] = [[]]
  mu ((t1, t2) : ts) = [o s1 s2 | s1 <- unify t1 t2, s2 <- mu [(subst (fst t) s1, subst (snd t) s1) | t <- ts]]

  -- |The 'unify' function unify a pair of types.
  unify :: Type -> Type -> [Substitution]
  unify (T x) (T y) = if x == y then [[]] else [[(x, T y)]]
  unify (T x) t = if elem x (ids t) then
                    error ("Unification Fails 1. (" ++ show (T x) ++ "=" ++ show t ++ ")")
                  else return [(x, t)] where
                   ids t
                    = case t of
                      T x -> [x]
                      t1 :-> t2 -> ids t1 ++ ids t2
                      _ -> []
  unify t (T x) = unify (T x) t
  unify (t1 :-> t2) (t3 :-> t4) = [o s1 s2 | s1 <- (unify t1 t3), s2 <- (unify (subst t2 s1) (subst t4 s1))]
  unify t s = if t == s then [[]] else error ("Unification Fails 2. (" ++ show t ++ "=" ++ show s ++ ")")