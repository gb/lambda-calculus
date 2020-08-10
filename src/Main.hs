module Main where

import Control.Monad.IO.Class
import System.Console.Haskeline
import LambdaCalculus.Core
import LambdaCalculus.Eval
import LambdaCalculus.Parse

-- REPL
main :: IO ()
main = do
  putStrLn "Please type a lambda expression:"
  runInputT defaultSettings repl

repl :: InputT IO ()
repl = do
  term <- getInputLine "λ> "
  case term of
    Nothing -> return ()
    Just lambdaTerm -> do
      evaluate lambdaTerm
      repl

evaluate :: MonadIO m => String -> InputT m ()
evaluate term = case parseLambda term of
  Right lambdaTerm -> mapM_ (outputStrLn . show) (expand lambdaTerm)
  Left err -> outputStrLn $ show err

expand :: LambdaTerm -> [LambdaTerm]
expand term = case betaReduction term of
  Just lambdaTerm -> term:(expand lambdaTerm)
  Nothing -> term:[]