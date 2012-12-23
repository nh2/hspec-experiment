{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE GADTs #-}

module Structure where

import           Control.Monad.Free
import qualified Data.Foldable as F
import qualified Data.Traversable as T

import Tests


-- Test chain functor for Free monad

data TestChain next = ChainEntry String AnyTest next
                    | ChainDescribe String (Free TestChain ()) next
                    deriving (Functor, Show)


-- Monadic test structure building

it :: (IsTest t typ) => String -> t -> Free TestChain ()
it desc test = liftF (ChainEntry desc (AnyTest (mkTest test)) ())

describe :: String -> Free TestChain () -> Free TestChain ()
describe desc childrenChain = liftF (ChainDescribe desc childrenChain ())


-- Actual test tree structure (and chain functor to test tree conversion)

data TestTree a = TestNode String a
                | Describe String [TestTree a]
                deriving (Show, Functor, F.Foldable, T.Traversable)

testTree :: Free TestChain () -> [TestTree AnyTest]
testTree (Free x) = case x of
  ChainEntry desc anyTest rest -> (TestNode desc anyTest) : testTree rest
  ChainDescribe desc children rest -> (Describe desc (testTree children)) : testTree rest
testTree (Pure ()) = []
