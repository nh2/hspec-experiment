{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}

module Clean where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Free
import           Control.Monad.Reader
import           Data.Tree
import           Data.Traversable
import qualified Data.Foldable as F
import           System.IO.Silently
import qualified Test.QuickCheck as QC
import           Test.QuickCheck.Property

import PureAndSo



instance Show Test where
  show t = case t of
    PureTest s t -> "PureTest " ++ s ++ " " ++ getDescription t
    SemiPureTest s t -> "SemiPureTest " ++ s ++ " " ++ getDescription t
    ImpureTest s t -> "ImpureTest " ++ s ++ " " ++ getDescription t

-- data TestChain next = ChainEntry (Partition t typ => TestG typ) next
--                     | ChainDescribe String (Free TestChain ()) next
--                     deriving (Functor)

data TestChain next = {-forall t typ . Partition t typ-} forall typ . SomeT typ => ChainEntry (TestG typ) next
                    | ChainDescribe String (Free TestChain ()) next

instance Functor TestChain where
  fmap f (ChainEntry testg next) = ChainEntry testg (f next)
  fmap f (ChainDescribe s ftc next) = ChainDescribe s ftc (f next)


it :: (Partition t typ) => String -> t -> Free TestChain ()
it desc test = liftF (ChainEntry (mkTestG test) ())

describe :: String -> Free TestChain () -> Free TestChain ()
describe desc childrenChain = liftF (ChainDescribe desc childrenChain ())


x1 = do
  it "test1" $ True
  it "test2" $ False

x2 = do
  it "test1" $ True
  describe "section1" $ do
    it "test2" $ False
    it "test3" $ False
  it "test4" $ True

x3 = do
  it "test1" $ True
  it "test2" $ (return True :: IO Bool)

data InTestNode = forall a . InTestNode { innode :: TestG a }

instance Show InTestNode where
  show (InTestNode t) = case t of
    MkPure t     -> "[pure] " ++ getDescription t
    MkSemiPure t -> "[semipure] " ++ getDescription t
    MkImpure t   -> "[impure] " ++ getDescription t


data TestTree a = TestNode a
                | Describe String [TestTree a]
                deriving (Show, Functor, F.Foldable, Traversable)

-- instance Show (TestTree a) where
--   show (TestNode a) = "TestNode"
--   show (Describe desc subs) = "Describe " ++ desc ++ " " ++ show subs


testTree :: Free TestChain () -> [TestTree InTestNode]
testTree (Free x) = case x of
  ChainEntry test rest -> (TestNode (InTestNode test)) : testTree rest
  ChainDescribe desc children rest -> (Describe desc (testTree children)) : testTree rest
testTree (Pure ()) = []

t = testTree

t1 = testTree x1
t2 = testTree x2
t3 = testTree x3


showTestTree :: [TestTree InTestNode] -> String
showTestTree = unlines . concatMap (indent more)
  where
    indent pad t = case t of
      TestNode (InTestNode test) -> case test of
        MkPure t     -> ["+ [pure] " ++ getDescription t] -- show test]
        MkSemiPure t -> ["+ [semipure] " ++ getDescription t] -- show test]
        MkImpure t   -> ["+ [impure] " ++ getDescription t] -- show test]
      Describe desc st -> ["- " ++ desc] ++ concatMap (map pad . indent (more . pad)) st
    more = ("  " ++)

printTestTree = putStr . showTestTree


-- TODO allow custom test return info
runTestTree :: [TestTree InTestNode] -> IO [TestTree (InTestNode, SpecResult)]
runTestTree = traverse $ \x -> case x of
  TestNode x@(InTestNode test) -> case test of
    -- TODO not use default settings
    MkPure t -> return $ TestNode (x, resultPure defaultSettings t)
    MkSemiPure t -> do result <- resultSemiPure defaultSettings t
                       return $ TestNode (x, result)
    MkImpure t -> do res <- resultImpure defaultSettings t
                     return $ TestNode (x, res)
