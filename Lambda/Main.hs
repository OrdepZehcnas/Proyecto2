{-|
Module      : Main
Description : The main file for our Pure Lambda Calculus calculator.
Copyright   : (c) Fernando Abigail Galicia-Mendoza, 2019
License     : GPL-3
Maintainer  : fernandogamen@ciencias.unam.mx
Stability   : experimental-education
Portability : POSIX
-}

import System.Environment
import PureLambda
import ParseLam

{-
This function transforms the "concrete syntax" to the abstract syntax
defined in the PureLambda script
-}
transform :: Stmt -> Lambda 
transform (Var x) = V x
transform (AppS t s) = App (transform t) (transform s)
transform (LS args t) = lamsArgs args (transform t) --In case of \x1.x2....xn -> e, we return \x1->\x2->...(\xn->t)
transform (FunS args t) = funS args (transform t) --Analogous as the previous case
transform (LetS x t1 t2) = Let x (transform t1) (transform t2)
transform (LetFunS f args t1 t2) = Let f (transform (FunS args t1)) (transform t2) --We unfold the syntax sugar for the functions with name.

lamsArgs :: [Identifier] -> Lambda -> Lambda
lamsArgs [x] t = L x t
lamsArgs (x:xs) t = L x (lamsArgs xs t)

funS :: [Identifier] -> Lambda -> Lambda
funS [x] t = Fun x t
funS (x:xs) t = Fun x (funS xs t)

-- A cute main
main = do
  args <- getArgs
  case args of
    [file] -> do
      x <- parseFile file
      let t = transform x
      putStrLn $ "Program:\n\n"++show t++"\n"
      putStrLn "Evaluation:\n"
      putStrLn $ show $ betas t
      putStrLn ""
    _ -> putStrLn "Error: Only put the Identifier of the file."