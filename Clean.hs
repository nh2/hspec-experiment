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
import           Data.Maybe
import           Data.Traversable hiding (mapM)
import qualified Data.Foldable as F
import           System.IO.Silently
import qualified Test.QuickCheck as QC
import           Test.QuickCheck.Property

import PureAndSo


-- Test existential

data AnyTest = forall a . AnyTest { innode :: Test a }

instance Show AnyTest where
  show (AnyTest t) = case t of
    MkPure t     -> "[pure] " ++ getDescription t
    MkSemiPure t -> "[semipure] " ++ getDescription t
    MkImpure t   -> "[impure] " ++ getDescription t


-- Test chain functor for Free monad

data TestChain next = ChainEntry AnyTest next
                    | ChainDescribe String (Free TestChain ()) next
                    deriving (Functor)


-- Test structure

it :: (IsTest t typ) => String -> t -> Free TestChain ()
it desc test = liftF (ChainEntry (AnyTest (mkTest test)) ())

describe :: String -> Free TestChain () -> Free TestChain ()
describe desc childrenChain = liftF (ChainDescribe desc childrenChain ())


-- Test chain functor to test tree

data TestTree a = TestNode a
                | Describe String [TestTree a]
                deriving (Show, Functor, F.Foldable, Traversable)

testTree :: Free TestChain () -> [TestTree AnyTest]
testTree (Free x) = case x of
  ChainEntry anyTest rest -> (TestNode anyTest) : testTree rest
  ChainDescribe desc children rest -> (Describe desc (testTree children)) : testTree rest
testTree (Pure ()) = []


-- Examples

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


t = testTree

t1 = testTree x1
t2 = testTree x2
t3 = testTree x3


showTestTree :: [TestTree AnyTest] -> String
showTestTree = unlines . concatMap (indent more)
  where
    indent pad t = case t of
      TestNode (AnyTest test) -> case test of
        MkPure t     -> ["+ [pure] " ++ getDescription t] -- show test]
        MkSemiPure t -> ["+ [semipure] " ++ getDescription t] -- show test]
        MkImpure t   -> ["+ [impure] " ++ getDescription t] -- show test]
      Describe desc st -> ["- " ++ desc] ++ concatMap (map pad . indent (more . pad)) st
    more = ("  " ++)

printTestTree = putStr . showTestTree


-- TODO allow custom test return info
resultTestTree :: [TestTree AnyTest] -> IO [TestTree (AnyTest, SpecResult)]
resultTestTree = traverse $ \x -> case x of
  TestNode x@(AnyTest test) -> case test of
    -- TODO not use default settings
    MkPure t -> return $ TestNode (x, resultPure defaultSettings t)
    MkSemiPure t -> do result <- resultSemiPure defaultSettings t
                       return $ TestNode (x, result)
    MkImpure t -> do res <- resultImpure defaultSettings t
                     return $ TestNode (x, res)
  Describe desc st -> do ress <- resultTestTree st
                         return $ Describe desc ress

runTestTree :: [TestTree AnyTest] -> IO ()
runTestTree ts = do
  resTree <- resultTestTree ts
  putStr (unlines $ concatMap (indent more) resTree)
   where
      indent :: (String -> String) -> TestTree (AnyTest, SpecResult) -> [String]
      indent pad node = case node of
        TestNode (AnyTest test, res) -> let r = resultToString res in case test of
          MkPure t     -> ["+ [pure] " ++ getDescription t ++ " ... " ++ r]
          MkSemiPure t -> ["+ [semipure] " ++ getDescription t ++ " ... " ++ r]
          MkImpure t   -> ["+ [impure] " ++ getDescription t ++ " ... " ++ r]
        Describe desc st -> let rss = map (map pad . indent (more . pad)) st
                             in ["- " ++ desc] ++ concat rss
      more = ("  " ++)


-- Pure version (skips non-pure tests and runs without IO)

resultTestTreePure :: [TestTree AnyTest] -> [TestTree (Test PureT, SpecResult)]
resultTestTreePure trees = catMaybes . flip map trees $ \x -> case x of
  TestNode (AnyTest test) -> case test of
    -- TODO not use default settings
    MkPure t -> Just $ TestNode (test, resultPure defaultSettings t)
    _        -> Nothing
  Describe desc st -> Just $ Describe desc (resultTestTreePure st)

runTestTreePure :: [TestTree AnyTest] -> String
runTestTreePure ts = do
  unlines $ concatMap (indent more) (resultTestTreePure ts)
   where
      indent :: (String -> String) -> TestTree (Test PureT, SpecResult) -> [String]
      indent pad node = case node of
        TestNode (test, res) -> let r = resultToString res in case test of
          MkPure t     -> ["+ [pure] " ++ getDescription t ++ " ... " ++ r]
        Describe desc st -> let rss = map (map pad . indent (more . pad)) st
                             in ["- " ++ desc] ++ concat rss
      more = ("  " ++)
