{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE GADTs #-}

-- | Describes the structure of tests.
-- Tests are assembled in monadic notation using `it` and `describe`.
--
-- Eventually, this describes a tree where describes are the inner nodes
-- and the actual tests are the leaves.
--
-- With the Tree being pure data, it can easily be filtered, re-ordered,
-- or modified.
-- Test nodes can contain tests of any of the three types Pure, SemiPure
-- or Impure (wrapped by the `AnyTest` existential), but can be
-- pattern-matched to reveal their type.
module Structure where

import           Control.Monad.Free
import qualified Data.Foldable as F
import qualified Data.Traversable as T

import Tests


-- Test chain functor for the Free Monad.
-- The allows us to get a tree "for free" using do-notation.

-- Inspired by:
-- http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html

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
