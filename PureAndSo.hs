{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}

module PureAndSo where

import           System.IO.Silently
import qualified Test.QuickCheck as QC
import           Test.QuickCheck.Property
import           Control.Applicative


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


class PureTest a where
  resultPure :: Settings -> a -> SpecResult

class SemiPureTest a where
  resultSemiPure :: Settings -> a -> IO SpecResult

class ImpureTest a where
  resultImpure :: Settings -> a -> IO SpecResult


data PureT = PureT
data SemiPureT = SemiPureT
data ImpureT = ImpureT

class SomeT a where

instance SomeT PureT
instance SomeT SemiPureT
instance SomeT ImpureT

class SomeT typ => IsTest test typ | test -> typ where
  mkTest :: test -> Test typ

instance IsTest Bool PureT where
  mkTest = MkPure
instance IsTest Property SemiPureT where
  mkTest = MkSemiPure
instance PureTest a => IsTest (IO a) ImpureT where
  mkTest = MkImpure


-- Possible alternative?: Put indrotduce + run function into GADT, change types (IO / non-IO) based on whether it's pure or not

data Test t where
  MkPure :: (IsTest t PureT, PureTest t) => t -> Test PureT
  MkSemiPure :: (IsTest t SemiPureT, SemiPureTest t) => t -> Test SemiPureT
  MkImpure :: (IsTest t ImpureT, ImpureTest t) => t -> Test ImpureT




instance PureTest Bool where
  resultPure _ True  = Ok
  resultPure _ False = Fail

instance SemiPureTest Property where
  resultSemiPure _settings p = do
    r <- silence (QC.quickCheckResult p)
    return $ case r of
      QC.Success {}               -> Ok
      f@(QC.Failure {})           -> FailMessage (QC.output f)
      QC.GaveUp {QC.numTests = n} -> FailMessage ("Gave up after " ++ show n ++ "tests")
      QC.NoExpectedFailure {}     -> FailMessage ("No expected failure")


instance (PureTest t) => ImpureTest (IO t) where
  resultImpure settings iob = resultPure settings <$> iob
