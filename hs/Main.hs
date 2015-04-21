import System.Console.Haskeline

import AST
import Analyze
import Parse
import ParseType
import ShowExpr
import Test.QuickCheck
import ASTGen
import Simplify

main = hello >> readEvalPrintLoop

hello :: IO ()
hello = do
  putStrLn "Fun Type Inferencer"

readEvalPrintLoop :: IO ()
readEvalPrintLoop = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "> "
      case minput of
        Nothing     -> return () -- EOF / control-d
        Just "exit" -> return ()
        Just line -> outputStrLn (process line) >> loop

process :: String -> String
process input = case parseExpr input of
  Left err -> show err
  Right expr -> typeInference expr

typeInference :: Expr -> String
typeInference expr = case runAnalyzer (analyzeExpr initialVars expr) of
  Left err -> err
  Right (t, env) -> show (simplify t)
