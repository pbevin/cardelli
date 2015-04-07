import System.Console.Readline

import AST
import Analyze
import Parse
import ParseType
import ShowExpr
import Test.QuickCheck
import ASTGen

main = hello >> readEvalPrintLoop

readEvalPrintLoop :: IO ()
readEvalPrintLoop = do
 maybeLine <- readline "> "
 case maybeLine of 
  Nothing     -> return () -- EOF / control-d
  Just "exit" -> return ()
  Just line -> do addHistory line
                  putStrLn $ typeInference line
                  readEvalPrintLoop

typeInference :: String -> String
typeInference input = case parseExpr input of
  Left err -> show err
  Right expr -> typeInference' expr

typeInference' :: Expr -> String
typeInference' expr = case runAnalyzer (analyzeExpr initialVars expr) of
  Left err -> err
  Right (t, env) -> show t

hello :: IO ()
hello = do
  putStrLn "Fun Type Inferencer"
