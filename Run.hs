{-# LANGUAGE GADTs #-}

-- | Displaying and running test suites.
module Run where

import           Data.Maybe
import qualified Data.Traversable as T

import Tests
import Structure


-- Test showing and running

showTestTree :: [TestTree AnyTest] -> String
showTestTree = unlines . concatMap (indent more)
  where
    indent pad t = case t of
      TestNode desc (AnyTest test) -> case test of
        MkPure _ _     -> ["+ [pure] " ++ desc]
        MkSemiPure _ _ -> ["+ [semipure] " ++ desc]
        MkImpure _ _   -> ["+ [impure] " ++ desc]
      Describe desc st -> ["- " ++ desc] ++ concatMap (map pad . indent (more . pad)) st
    more = ("  " ++)


printTestTree :: [TestTree AnyTest] -> IO ()
printTestTree = putStr . showTestTree


-- TODO allow custom test return info
resultTestTree :: [TestTree AnyTest] -> IO [TestTree (AnyTest, SpecResult)]
resultTestTree = T.traverse $ \tree -> case tree of
  TestNode desc x@(AnyTest test) -> case test of
    -- TODO not use default settings
    MkPure resultFun t -> return $ TestNode desc (x, resultFun defaultSettings t)
    MkSemiPure resultFun t -> do result <- resultFun defaultSettings t
                                 return $ TestNode desc (x, result)
    MkImpure resultFun t -> do res <- resultFun defaultSettings t
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
        TestNode desc (anyTest, res) -> let r = resultToString res
                                         in ["+ " ++ show anyTest ++ " " ++ desc ++ " ... " ++ r]
        Describe desc st -> let rss = map (map pad . indent (more . pad)) st
                             in ["- " ++ desc] ++ concat rss
      more = ("  " ++)


-- Pure version (skips non-pure tests and runs without IO)
--
-- This demonstrates how the `Test` GADT allows us to treat pure, semi-pure and
-- impure tests differently.
-- Here we select only the pure tests, which allows us to run them without IO.

resultTestTreePure :: [TestTree AnyTest] -> [TestTree (Test PureT, SpecResult)]
resultTestTreePure trees = catMaybes . flip map trees $ \x -> case x of
  TestNode desc (AnyTest test) -> case test of
    -- TODO not use default settings
    MkPure resultFun t -> Just $ TestNode desc (test, resultFun defaultSettings t)
    _                  -> Nothing
  Describe desc st -> Just $ Describe desc (resultTestTreePure st)


runTestTreePure :: [TestTree AnyTest] -> String
runTestTreePure ts = do
  unlines $ concatMap (indent more) (resultTestTreePure ts)
   where
      indent :: (String -> String) -> TestTree (Test PureT, SpecResult) -> [String]
      indent pad node = case node of
        TestNode desc (_test, res) -> let r = resultToString res
                                       in ["+ [pure] " ++ desc ++ " ... " ++ r]
        Describe desc st -> let rss = map (map pad . indent (more . pad)) st
                             in ["- " ++ desc] ++ concat rss
      more = ("  " ++)
