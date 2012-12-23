module Examples where

import Test.QuickCheck

import Structure
import Run


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

x4 = do
  it "test1" $ True
  it "test2" $ property $
    \x -> x || not x == True

t1 = testTree x1
t2 = testTree x2
t3 = testTree x3
t4 = testTree x4

printExamples = mapM_ printTestTree [t1, t2, t3, t4]
runExamples = mapM_ runTestTree [t1, t2, t3, t4]
