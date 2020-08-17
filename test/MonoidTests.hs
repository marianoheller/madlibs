module MonoidTests where

import Data.Monoid
import Lib
import Test.QuickCheck

type S = String

type B = Bool

semigrpAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigrpAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity m = mempty <> m == m

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity m = m <> mempty == m

combineSemigroupAssoc ::
  (Eq b, Show b, Semigroup b) =>
  a ->
  Combine a b ->
  Combine a b ->
  Combine a b ->
  Bool
combineSemigroupAssoc x (Combine f) (Combine g) (Combine h) =
  (f x <> (g x <> h x)) == ((f x <> g x) <> h x)

compSemigroupAssoc ::
  (Eq a, Show a, Semigroup a) =>
  a ->
  Comp a ->
  Comp a ->
  Comp a ->
  Bool
compSemigroupAssoc x (Comp f) (Comp g) (Comp h) =
  (f x <> (g x <> h x)) == ((f x <> g x) <> h x)

monoidMain :: IO ()
monoidMain = do
  quickCheck (semigrpAssoc :: S -> S -> S -> B)
  quickCheck (monoidLeftIdentity :: S -> B)
  quickCheck (monoidRightIdentity :: S -> B)

  -- Optional
  quickCheck (semigrpAssoc :: First' S -> First' S -> First' S -> B)
  quickCheck (monoidLeftIdentity :: First' S -> B)

  -- Trivial
  quickCheck (semigrpAssoc :: Trivial -> Trivial -> Trivial -> B)

  -- Identity
  quickCheck (semigrpAssoc :: Identity S -> Identity S -> Identity S -> B)

  -- Two
  quickCheck (semigrpAssoc :: Two S S -> Two S S -> Two S S -> B)

  -- BoolConj
  quickCheck (semigrpAssoc :: BoolConj -> BoolConj -> BoolConj -> B)

  -- Combine
  quickCheck (combineSemigroupAssoc :: String -> Combine S [Int] -> Combine S [Int] -> Combine S [Int] -> Bool)

  -- Comp
  quickCheck (compSemigroupAssoc :: S -> Comp S -> Comp S -> Comp S -> Bool)

  -- Validation
  quickCheck (semigrpAssoc :: Two S S -> Two S S -> Two S S -> B)
