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


-- class Check a where
--   mkCheck :: String -> a -> Test

-- instance Check Bool where
--   mkCheck desc b = BoolTest desc b

-- instance (Check c) => Check (IO c) where
--   mkCheck desc ioc = IOTest desc (mkCheck "" <$> ioc)


-- data Test = BoolTest String Bool
--           -- | forall c . IOTest String (Check c => IO c)
--           | IOTest String (IO Test)

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

-- deriving instance Show a => Show (TestChain a)

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


data TestTree a = forall a . TestNode (TestG a)
                | Describe String [TestTree a]
                -- deriving (Show, Functor, F.Foldable, Traversable)

-- deriving instance Show a => Show (TestTree a)
instance Show (TestTree a) where
  show (TestNode a) = "TestNode"
  show (Describe desc subs) = "Describe " ++ desc ++ " " ++ show subs

-- type Tests = TestTree (TestG a)

testTree :: Free TestChain () -> [TestTree (TestG typ)]
testTree (Free x) = case x of
  ChainEntry (test :: TestG typ) rest -> (TestNode test) : testTree rest
  ChainDescribe desc children rest -> (Describe desc (testTree children)) : testTree rest
testTree (Pure ()) = []

t = testTree

t1 = testTree x1
t2 = testTree x2
t3 = testTree x3


showTestTree :: [TestTree (TestG a)] -> String
showTestTree = unlines . concatMap (indent more)
  where
    indent pad t = case t of
      TestNode (MkPure t)    -> ["+ [pure] " ++ getDescription t] -- show test]
      TestNode (MkSemiPure t)    -> ["+ [semipure] " ++ getDescription t] -- show test]
      TestNode (MkImpure t)    -> ["+ [impure] " ++ getDescription t] -- show test]
      Describe desc st -> ["- " ++ desc] ++ concatMap (map pad . indent (more . pad)) st
    more = ("  " ++)

printTestTree = putStr . showTestTree


-- TODO allow custom test return info
runTestTree :: [TestTree (TestG a)] -> IO [TestTree (TestG a, SpecResult)]
-- runTestTree = mapM run
--   where
--     run t = case t of
--       TestNode test -> case test of
--         -- BoolTest desc b ->
runTestTree = traverse $ \x -> case x of
  TestNode test -> case test of
    -- TODO not use default settings
    MkPure t -> undefined -- return $ TestNode (test, resultPure defaultSettings t)
    -- IOTest desc iotest -> do b <- iotest
    --                          return $ TestNode (test, b, if b then Nothing else Just "returned false")
