{-|
Module      : Main
Description : The main file for our Mini Haskell evaluator.
Copyright   : (c) Fernando Abigail Galicia-Mendoza, 2019
License     : GPL-3
Maintainer  : fernandogamen@ciencias.unam.mx
Stability   : experimental-education
Portability : POSIX
-}

import System.Environment
import W
import ParserMinHS
import MiniHs

-- | The 'transform' function takes a program parsed (called, statement)
-- and returns the abstract syntax of a MiniHaskell program. 
-- [NOTE]: The function deletes the sugar syntax.
transform :: Stmt -> MiniHsU
transform e = error "Funcion no implementada" 

-- | The main function of the evaluator.
main = do
  args <- getArgs
  case args of
    [file] -> do
      x <- parseFile file
      let e = transform x
      putStrLn $ "Your program is:\n\n"++show e++"\n"
      let Assertion (c,e',t) = w e
      putStrLn $ "Typed Program:\n\n"++show (Assertion (c,e',t))++"\n"
      putStrLn "Evaluation:\n"
      putStrLn $ show $ evals e
      putStrLn ""
    _ -> putStrLn "Error: Only put the name of the file."