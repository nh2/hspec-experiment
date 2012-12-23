{-# LANGUAGE GADTs, FlexibleInstances, ExistentialQuantification, FunctionalDependencies, FlexibleContexts #-}

-- | Defines what tests are.
-- Tests have a run function and belong to one of three types: Pure, SemiPure, or Impure.
--
-- * Pure tests are tests without IO side effects.
-- * SemiPure tests have read-only side effects and must not influence each other.
--   Example: Normal QuickCheck properties.
-- * Impure tests have IO side effects that may influence each other.
--
-- Pure and SemiPure tests should be expected to run in parallel.
-- Impure tests should be expected not to run in parallel unless annotated explicitly.
--
-- Some more examples for choosing between SemiPure and Impure:
--
-- * Writing something to a file, then deleting that file.
--   -> Impure, because parallel tests could fail to write to the same file.
-- * Connecting to a socket and asking for information.
--   -> Impure, because you might run out of sockets when run in parallel.
-- * A QuickCheck property about pure functions that needs random numbers to run.
--   -> SemiPure, because they do not influence each other.
module Tests where

import           System.IO.Silently
import qualified Test.QuickCheck as QC
import           Test.QuickCheck.Property


-- Settings

data Settings = Settings

defaultSettings :: Settings
defaultSettings = Settings


-- Results

data SpecResult = Ok
                | Fail
                | FailMessage String
                deriving (Eq, Show)


resultToString :: SpecResult -> String
resultToString r = case r of
  Ok            -> "OK"
  Fail          -> "FAIL"
  FailMessage m -> "FAIL (" ++ m ++ ")"


-- Test types

data PureT     = PureT     deriving (Show)
data SemiPureT = SemiPureT deriving (Show)
data ImpureT   = ImpureT   deriving (Show)

class SomeT a where

instance SomeT PureT
instance SomeT SemiPureT
instance SomeT ImpureT


-- IsTest class (fundep for every test can only be one of pure/semipure/impure)

class SomeT typ => IsTest test typ | test -> typ where
  mkTest :: test -> Test typ


-- TestRunFunction aliases

type TestRunFunctionPure a     = Settings -> a -> SpecResult
type TestRunFunctionSemiPure a = Settings -> a -> IO SpecResult
type TestRunFunctionImpure a   = Settings -> a -> IO SpecResult


-- Test GADT
-- Contains the test itself and a way to run it to a result.

data Test t where
  MkPure     :: (IsTest t PureT)     => TestRunFunctionPure t     -> t -> Test PureT
  MkSemiPure :: (IsTest t SemiPureT) => TestRunFunctionSemiPure t -> t -> Test SemiPureT
  MkImpure   :: (IsTest t ImpureT)   => TestRunFunctionImpure t   -> t -> Test ImpureT


instance Show (Test t) where
  show test = case test of
    MkPure _ _     -> "[pure]"
    MkSemiPure _ _ -> "[semipure]"
    MkImpure _ _   -> "[impure]"


-- Test existential

data AnyTest = forall a . AnyTest { innode :: Test a }

instance Show AnyTest where
  show (AnyTest test) = show test


-- IsTest instances

resultBool :: TestRunFunctionPure Bool
resultBool _ True  = Ok
resultBool _ False = Fail


resultProperty :: TestRunFunctionSemiPure Property
resultProperty _settings p = do
  r <- silence (QC.quickCheckResult p)
  return $ case r of
    QC.Success {}               -> Ok
    f@(QC.Failure {})           -> FailMessage (QC.output f)
    QC.GaveUp {QC.numTests = n} -> FailMessage ("Gave up after " ++ show n ++ "tests")
    QC.NoExpectedFailure {}     -> FailMessage ("No expected failure")


instance IsTest Bool PureT where
  mkTest = MkPure resultBool

instance IsTest Property SemiPureT where
  mkTest = MkSemiPure resultProperty

instance IsTest a PureT => IsTest (IO a) ImpureT where
  mkTest = MkImpure f
    where
      f :: IsTest a PureT => TestRunFunctionImpure (IO a)
      f settings iotest = do puretest <- iotest
                             case mkTest puretest of
                               MkPure run t -> return $ run settings t
