{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes, StandaloneDeriving #-}

module Main where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Free
import           System.IO.Silently
import qualified Test.QuickCheck as QC
import           Test.QuickCheck.Property


data Settings = Settings

defaultSettings = Settings

data SpecResult = Ok
                | Fail
                | FailMessage String
                deriving (Eq, Show)


class PureTest a where
  resultPure :: Settings -> a -> SpecResult

class SemiPureTest a where
  resultSemiPure :: Settings -> a -> IO SpecResult

class ImpureTest a where
  resultImpure :: Settings -> a -> IO SpecResult


instance PureTest Bool where
  resultPure _ True  = Ok
  resultPure _ False = Fail

instance SemiPureTest Property where
  resultSemiPure _set p = do
    r <- silence (QC.quickCheckResult p)
    return $ case r of
      QC.Success {}               -> Ok
      f@(QC.Failure {})           -> FailMessage (QC.output f)
      QC.GaveUp {QC.numTests = n} -> FailMessage ("Gave up after " ++ show n ++ "tests")
      QC.NoExpectedFailure {}     -> FailMessage ("No expected failure")


instance ImpureTest (IO Bool) where
  resultImpure set iob = resultPure set <$> iob

type Desc = Maybe String

data SomePureTest = forall t . PureTest t => SomePureTest t
data SomeSemiPureTest = forall t . SemiPureTest t => SomeSemiPureTest t
data SomeImpureTest = forall t . ImpureTest t => SomeImpureTest t

data Test = Section [Test]
          | PureT SomePureTest
          | SemiPureT SomeSemiPureTest
          | ImpureT SomeImpureTest

data Plan a next = Step Desc Test next
                 | Done a
                 deriving (Functor)


done :: Free (Plan ()) a
done = Free (Done ())

step :: String -> Test -> Free (Plan a) ()
step d t = Free (Step (Just d) t (Pure ()))


plan1 = do
  step "step 1" (PureT $ SomePureTest True)
  step "step 2" (PureT $ SomePureTest True)
  step "step 3" (ImpureT $ SomeImpureTest (not . null <$> getLine))
  -- done
  -- return 3
  step "step 4" (PureT $ SomePureTest True)


showPlan :: Free (Plan t) () -> String
showPlan (Free (Step msg _ x)) = "step " ++ show msg ++ "\n" ++ showPlan x
showPlan (Free (Done _)) = ""
showPlan (Pure ()) = ""

printPlan = putStr . showPlan

runPlan :: Free (Plan t) () -> IO ()
runPlan (Free (Step msg t rest)) = do
  putStr $ "step " ++ show msg ++ " ... "
  runTest t
  runPlan rest
  where
    runTest t = case t of
      Section tests                 -> -- TODO run tests in parallel
                                       mapM_ runTest tests
      PureT (SomePureTest pureTest) -> putStrLn . resultToString $ resultPure defaultSettings pureTest

      SemiPureT (SomeSemiPureTest semiPureTest) -> putStrLn . resultToString =<< resultSemiPure defaultSettings semiPureTest
      ImpureT (SomeImpureTest impureTest)       -> putStrLn . resultToString =<< resultImpure   defaultSettings impureTest
    resultToString r = case r of
      Ok            -> "OK"
      Fail          -> "FAIL"
      FailMessage m -> "FAIL (" ++ m ++ ")"
runPlan (Free (Done _)) = return ()
runPlan (Pure ()) = return ()


step' :: (ImpureTest ipt) => String -> ipt -> Free (Plan a) ()
step' d t = Free (Step (Just d) (ImpureT $ SomeImpureTest t) (Pure ()))

plan2 = do
  step' "step 1" (not . null <$> getLine)
  step' "step 2" (not . null <$> getLine)


-- step'a :: (ImpureTest ipt) => String -> ipt -> Free (Plan a) a
-- step'a d t = Free (Step (Just d) (ImpureT $ SomeImpureTest t) (Pure ()))

-- loginPlan = do
--   login_handle <- step'a "log in step" $ do
--     l <- getLine
--     -- TODO assert something about l (not . null)
--     return l
--   b <- step'a "print step" $ do
--     print login_handle
--     return 4
--   return ()



data Test2 = Section2 [Test]
           | PureT2 SomePureTest
           | SemiPureT2 SomeSemiPureTest
           | ImpureT2 SomeImpureTest


data Plan2 a next = Step2 Desc Test2 next
                  | Done2
                  deriving (Functor)


data Test3 = BoolTest String Bool
           deriving (Show)

data TestTree = TestNode Test3
              | Describe String [TestTree]
              deriving (Show)

data TestChain next = ChainEntry Test3 next
                    | ChainDescribe String (Free TestChain ()) next
                    deriving (Functor)

-- deriving instance Show a => Show (TestChain a)

it :: String -> Bool -> Free TestChain ()
it desc b = liftF (ChainEntry (BoolTest desc b) ())

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

structure :: Free TestChain () -> [TestTree]
structure (Free x) = case x of
  ChainEntry test rest -> (TestNode test) : structure rest
  ChainDescribe desc children rest -> (Describe desc (structure children)) : structure rest
structure (Pure ()) = []

sx1 = structure x1
sx2 = structure x2


-- output :: a -> Free (Toy a) ()
-- output x = Free (Output x (Pure ()))

-- bell :: Free (Toy a) ()
-- bell = Free (Bell (Pure ()))

-- done :: Free (Toy a) r
-- done = Free Done


data Direction = Forward | Backward
data Image = Image

data Interaction next =
    Look Direction (Image -> next)
  | Fire Direction next
  | ReadLine (String -> next)
  | WriteLine String (Bool -> next)
  | PrintLine String next
  deriving (Functor)

-- type Program = Free Interaction

easyToAnger :: Free Interaction a
easyToAnger = Free $ ReadLine $ \s -> case s of
    "No" -> Free $ Fire Forward
          $ Free $ WriteLine "Take that!" (\_ -> easyToAnger)
    _    -> easyToAnger


look :: Direction -> Free Interaction Image
look dir = liftF (Look dir id)

fire :: Direction -> Free Interaction ()
fire dir = liftF (Fire dir ())

liftF' :: Functor f => f a -> Free f a
liftF' fa = Free (fmap (\a -> Pure a) fa)

readLine :: Free Interaction String
-- readLine = liftF (ReadLine id)
-- readLine = Free (ReadLine ((\l -> Pure l) . id))
readLine = Free (ReadLine (\l -> Pure l))
-- readLine = Free (ReadLine Pure)

-- do
--   l <- readLine
--   printLine l

-- readLine >>= (\l -> printLine l)

-- Free (ReadLine (\s -> Pure s)) >>= (\l -> Free (PrintLine l (Pure ()))

-- Pure a     >>= f = f a
-- Free funct >>= f = Free (fmap (>>= f) funct)

-- Free (fmap (>>= f                                 )  funct                   )
-- - plug in
-- Free (fmap (>>= (\s -> Free (PrintLine s (Pure ())) (ReadLine (\l -> Pure l)))
-- - do the fmap
-- Free (ReadLine (\s -> Pure s >>= (\l -> Free (PrintLine l (Pure ())))
-- - do the >>= on the Pure
-- Free (ReadLine (\s -> Free (PrintLine s (Pure ()))
-- - the fmap-bind "fmap (>>= f)" jumps across the functors and monads
--   all the way down to the Pure, where the f is ultimately inserted.
--   The f contains a new Pure.

writeLine :: String -> Free Interaction Bool
writeLine s = liftF (WriteLine s id)

printLine :: String -> Free Interaction ()
-- printLine s = liftF (PrintLine s ())
printLine s = Free (PrintLine s (Pure ()))


easyToAnger' :: Free Interaction a
easyToAnger' = forever $ do
    str <- readLine
    when (str == "No") $ do
        fire Forward
        -- Ignore the Bool returned by writeLine
        _ <- writeLine "Take that!"
        return ()

-- runSpec :: Spec -> IO ()
-- runSpec = undefined

-- drySpec :: Spec -> String
-- drySpec = undefined


-- main = runSpec $ BoolSpec True

main = undefined
