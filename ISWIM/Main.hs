{-|
Module      : Main
Description : The main file for our Pure Lambda Calculus calculator.
Copyright   : (c) Fernando A. Galicia Mendoza, 2019
License     : GPL-3
Maintainer  : fernandogamen@ciencias.unam.mx
Stability   : experimental-education
Portability : POSIX
-}

import System.Environment
import PureLambda
import ParseLam

transform :: Stmt -> Lambda 
transform (Var x) = V x
transform (AppS t s) = App (transform t) (transform s)
transform (LS x t) = L x (transform t)
transform (LetS x t1 t2) = Let x (transform t1) (transform t2)
transform (LetFunS f x t1 t2) = LetFun f x (transform t1) (transform t2)

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
    _ -> putStrLn "Error: Only put the name of the file."