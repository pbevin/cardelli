module TypeGen where

import Control.Applicative
import Test.QuickCheck
import Type


newtype ConcreteType = ConcreteType { fromCT :: String } deriving Show
newtype AbstractType = AbstractType { fromAT :: String } deriving Show


instance Arbitrary Type where
  arbitrary = sized arbType

instance Arbitrary AbstractType where
  arbitrary = do
    x <- elements $ ['a'..'z']
    xs <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']
    return $ AbstractType (x:xs)

instance Arbitrary ConcreteType where
  arbitrary = do
    x <- elements ['A'..'Z']
    xs <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']
    return $ ConcreteType (x:xs)

arbType :: Int -> Gen Type
arbType 0 = oneof [ BasicType <$> fromCT <$> arbitrary, TypeVariable <$> fromAT <$> arbitrary]
arbType n = oneof [ TypeOperator "->" <$> sequence [subtree, subtree],
                    TypeOperator "[]" <$> sequence [subtree],
                    TypeOperator "," <$> sequence [subtree, subtree] ]
    where subtree = arbType (n `div` 2)
