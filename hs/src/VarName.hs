module VarName where

import Data.Map (Map)
import qualified Data.Map as Map


newtype VarName = VarName { fromVarName :: String }
  deriving (Eq, Ord)

instance Show VarName where
  show (VarName a) = a

varNameMap :: [(String, b)] -> Map VarName b
varNameMap = Map.fromList . map tt
  where tt (a,b) = (VarName a, b)
