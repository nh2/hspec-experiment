{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}

module Tests where

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


type ResultFunctionPure a = Settings -> a -> SpecResult
type ResultFunctionSemiPure a = Settings -> a -> IO SpecResult
type ResultFunctionImpure a = Settings -> a -> IO SpecResult


data Test t where
  MkPure     :: (IsTest t PureT)     => ResultFunctionPure t     -> t -> Test PureT
  MkSemiPure :: (IsTest t SemiPureT) => ResultFunctionSemiPure t -> t -> Test SemiPureT
  MkImpure   :: (IsTest t ImpureT)   => ResultFunctionImpure t   -> t -> Test ImpureT


-- IsTest instances

resultBool :: ResultFunctionPure Bool
resultBool _ True  = Ok
resultBool _ False = Fail


resultProperty :: ResultFunctionSemiPure Property
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
      f :: IsTest a PureT => ResultFunctionImpure (IO a)
      f settings iotest = do puretest <- iotest
                             case mkTest puretest of
                               MkPure run t -> return $ run settings t
