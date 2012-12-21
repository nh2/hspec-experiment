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

defaultSettings = Settings

data SpecResult = Ok
                | Fail
                | FailMessage String
                deriving (Eq, Show)


class Descriptable a where
  getDescription :: a -> String
  -- mkTest :: String -> a -> Test


instance Descriptable Bool where
  getDescription = show
  -- mkTest desc b = PureTest desc b

instance Descriptable Property where
  getDescription _ = "Property"
  -- mkTest desc p = SemiPureTest desc p

instance (Descriptable a) => Descriptable (IO a) where
  getDescription _ = "IO"
  -- mkTest desc io = undefined -- ImpureTest desc io


class (Descriptable a) => PureTest a where
  resultPure :: Settings -> a -> SpecResult

class (Descriptable a) => SemiPureTest a where
  resultSemiPure :: Settings -> a -> IO SpecResult

class (Descriptable a) => ImpureTest a where
  resultImpure :: Settings -> a -> IO SpecResult


-- TODO use GADT?
data Test = forall t . PureTest t => PureTest String t
          | forall t . SemiPureTest t => SemiPureTest String t
          | forall t . ImpureTest t => ImpureTest String t

-- data TestType = Pure | SemiPure | Impure
data PureT = PureT
data SemiPureT = SemiPureT
data ImpureT = ImpureT

class SomeT a where

instance SomeT PureT
instance SomeT SemiPureT
instance SomeT ImpureT

class SomeT typ => Partition test typ | test -> typ where
  mkTestG :: test -> TestG typ

instance Partition Bool PureT where
  mkTestG = MkPure
instance Partition Property SemiPureT where
  mkTestG = MkSemiPure
instance PureTest a => Partition (IO a) ImpureT where
  mkTestG = MkImpure


-- data TestG a where
--   MkPure :: PureTest t => t -> TestG PureT
--   MkSemiPure :: SemiPureTest t => t -> TestG SemiPureT
--   MkImpure :: ImpureTest t => t -> TestG ImpureT

-- data TestG a where
--   MkPure :: PureTest t => t -> TestG t
--   MkSemiPure :: SemiPureTest t => t -> TestG t
--   MkImpure :: ImpureTest t => t -> TestG t


-- class TestableG a where
--   mkTestG :: a -> TestG a

-- instance TestableG Bool where
--   mkTestG b = MkPure b

-- x = mkTestG True


-- Possible alternative?: Put indrotduce + run function into GADT, change types (IO / non-IO) based on whether it's pure or not

data TestG t where
  MkPure :: (Partition t PureT, PureTest t) => t -> TestG PureT
  MkSemiPure :: (Partition t SemiPureT, SemiPureTest t) => t -> TestG SemiPureT
  MkImpure :: (Partition t ImpureT, ImpureTest t) => t -> TestG ImpureT




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


-- instance ImpureTest (IO Bool) where
--   resultImpure settings iob = resultPure settings <$> iob

instance (PureTest t) => ImpureTest (IO t) where
  resultImpure settings iob = resultPure settings <$> iob

-- instance (SemiPureTest t) => ImpureTest (IO t) where
--   resultImpure settings iob = resultSemiPure settings <$> iob

-- instance (PureTest t) => ImpureTest (IO t) where
--   resultImpure settings iob = resultPure settings <$> iob

