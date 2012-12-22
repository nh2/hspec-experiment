{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}

module PureAndSo where

import           System.IO.Silently
import qualified Test.QuickCheck as QC
import           Test.QuickCheck.Property


data Settings = Settings

defaultSettings :: Settings
defaultSettings = Settings

data SpecResult = Ok
                | Fail
                | FailMessage String
                deriving (Eq, Show)


resultToString :: SpecResult -> String
resultToString r = case r of
  Ok            -> "OK"
  Fail          -> "FAIL"
  FailMessage m -> "FAIL (" ++ m ++ ")"



data PureT = PureT
data SemiPureT = SemiPureT
data ImpureT = ImpureT

class SomeT a where

instance SomeT PureT
instance SomeT SemiPureT
instance SomeT ImpureT

class SomeT typ => IsTest test typ | test -> typ where
  mkTest :: test -> Test typ

-- Possible alternative?: Put indrotduce + run function into GADT, change types (IO / non-IO) based on whether it's pure or not


type RunFunctionPure a = Settings -> a -> SpecResult
type RunFunctionSemiPure a = Settings -> a -> IO SpecResult
type RunFunctionImpure a = Settings -> a -> IO SpecResult


data Test t where
  MkPure     :: (IsTest t PureT)     => RunFunctionPure t     -> t -> Test PureT
  MkSemiPure :: (IsTest t SemiPureT) => RunFunctionSemiPure t -> t -> Test SemiPureT
  MkImpure   :: (IsTest t ImpureT)   => RunFunctionImpure t   -> t -> Test ImpureT

-- resultPure :: (IsTest t PureT) => Test PureT -> RunFunctionPure t
-- resultPure (MkPure r _) = r
-- resultSemiPure (MkSemiPure r _) = r
-- resultImpure (MkImpure r _) = r


-- IsTest instances

resultBool :: RunFunctionPure Bool
resultBool _ True  = Ok
resultBool _ False = Fail


resultProperty :: RunFunctionSemiPure Property
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
      f :: IsTest a PureT => RunFunctionImpure (IO a)
      f settings iotest = do puretest <- iotest
                             case mkTest puretest of
                               MkPure run t -> return $ run settings t
