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
import           Data.Maybe
import           Data.Traversable hiding (mapM)
import qualified Data.Foldable as F

import PureAndSo


-- Test existential

data AnyTest = forall a . AnyTest { innode :: Test a }

instance Show AnyTest where
  show (AnyTest test) = case test of
    MkPure t     -> "[pure] " ++ getDescription t
    MkSemiPure t -> "[semipure] " ++ getDescription t
    MkImpure t   -> "[impure] " ++ getDescription t


-- Test chain functor for Free monad

data TestChain next = ChainEntry String AnyTest next
                    | ChainDescribe String (Free TestChain ()) next
                    deriving (Functor)


-- Test structure

it :: (IsTest t typ) => String -> t -> Free TestChain ()
it desc test = liftF (ChainEntry desc (AnyTest (mkTest test)) ())

describe :: String -> Free TestChain () -> Free TestChain ()
describe desc childrenChain = liftF (ChainDescribe desc childrenChain ())


-- Test chain functor to test tree

data TestTree a = TestNode String a
                | Describe String [TestTree a]
                deriving (Show, Functor, F.Foldable, Traversable)

testTree :: Free TestChain () -> [TestTree AnyTest]
testTree (Free x) = case x of
  ChainEntry desc anyTest rest -> (TestNode desc anyTest) : testTree rest
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


t1 = testTree x1
t2 = testTree x2
t3 = testTree x3


showTestTree :: [TestTree AnyTest] -> String
showTestTree = unlines . concatMap (indent more)
  where
    indent pad t = case t of
      TestNode desc (AnyTest test) -> case test of
        MkPure t     -> ["+ [pure] " ++ desc] -- show test]
        MkSemiPure t -> ["+ [semipure] " ++ desc] -- show test]
        MkImpure t   -> ["+ [impure] " ++ desc] -- show test]
      Describe desc st -> ["- " ++ desc] ++ concatMap (map pad . indent (more . pad)) st
    more = ("  " ++)

printTestTree = putStr . showTestTree


-- TODO allow custom test return info
resultTestTree :: [TestTree AnyTest] -> IO [TestTree (AnyTest, SpecResult)]
resultTestTree = traverse $ \x -> case x of
  TestNode desc x@(AnyTest test) -> case test of
    -- TODO not use default settings
    MkPure t -> return $ TestNode desc (x, resultPure defaultSettings t)
    MkSemiPure t -> do result <- resultSemiPure defaultSettings t
                       return $ TestNode desc (x, result)
    MkImpure t -> do res <- resultImpure defaultSettings t
                     return $ TestNode desc (x, res)
  Describe desc st -> do ress <- resultTestTree st
                         return $ Describe desc ress

runTestTree :: [TestTree AnyTest] -> IO ()
runTestTree ts = do
  resTree <- resultTestTree ts
  putStr (unlines $ concatMap (indent more) resTree)
   where
      indent :: (String -> String) -> TestTree (AnyTest, SpecResult) -> [String]
      indent pad node = case node of
        TestNode desc (AnyTest test, res) -> let r = resultToString res in case test of
          MkPure t     -> ["+ [pure] " ++ desc ++ " ... " ++ r]
          MkSemiPure t -> ["+ [semipure] " ++ desc ++ " ... " ++ r]
          MkImpure t   -> ["+ [impure] " ++ desc ++ " ... " ++ r]
        Describe desc st -> let rss = map (map pad . indent (more . pad)) st
                             in ["- " ++ desc] ++ concat rss
      more = ("  " ++)


-- Pure version (skips non-pure tests and runs without IO)

resultTestTreePure :: [TestTree AnyTest] -> [TestTree (Test PureT, SpecResult)]
resultTestTreePure trees = catMaybes . flip map trees $ \x -> case x of
  TestNode desc (AnyTest test) -> case test of
    -- TODO not use default settings
    MkPure t -> Just $ TestNode desc (test, resultPure defaultSettings t)
    _        -> Nothing
  Describe desc st -> Just $ Describe desc (resultTestTreePure st)

runTestTreePure :: [TestTree AnyTest] -> String
runTestTreePure ts = do
  unlines $ concatMap (indent more) (resultTestTreePure ts)
   where
      indent :: (String -> String) -> TestTree (Test PureT, SpecResult) -> [String]
      indent pad node = case node of
        TestNode desc (test, res) -> let r = resultToString res in case test of
          MkPure t     -> ["+ [pure] " ++ getDescription t ++ " ... " ++ r]
        Describe desc st -> let rss = map (map pad . indent (more . pad)) st
                             in ["- " ++ desc] ++ concat rss
      more = ("  " ++)
