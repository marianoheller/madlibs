module FunctorTests where

import Data.Monoid
import Lib
import Test.QuickCheck

type S = String

type B = Bool

functorIdentity :: (Eq (f a), Functor f) => f a -> Bool
functorIdentity f = fmap id f == id f

functorCompose :: (Eq (f c), Functor f) => Fun a b -> Fun b c -> f a -> Bool
functorCompose (Fun _ f) (Fun _ g) x = fmap (g . f) x == (fmap g (fmap f x))

functorMain :: IO ()
functorMain = do
  -- Identity
  quickCheck (functorIdentity :: Identity S -> B)
  quickCheck (functorCompose :: (Fun S S) -> (Fun S S) -> Identity S -> B)

  -- Two
  quickCheck (functorIdentity :: Two S S -> B)
  quickCheck (functorCompose :: (Fun S S) -> (Fun S S) -> Two S S -> B)

  -- TalkToMe
  quickCheck (functorIdentity :: TalkToMe S -> B)
  quickCheck (functorCompose :: (Fun S S) -> (Fun S S) -> TalkToMe S -> B)

  -- List
  quickCheck (functorIdentity :: List S -> B)
  quickCheck (functorCompose :: (Fun S S) -> (Fun S S) -> List S -> B)
