module Lib where

import Control.Applicative as Ap
import Data.Bifunctor as Bi
import Data.Monoid
import Test.QuickCheck

type Verb = String

type Adjective = String

type Adverb = String

type Noun = String

type Exclamation = String

madlibbin ::
  Exclamation ->
  Adverb ->
  Noun ->
  Adjective ->
  String
madlibbin e adv noun adj =
  mconcat
    [ e,
      "! he said ",
      adv,
      " as he jumped into his car ",
      noun,
      " and drove off with his ",
      adj,
      " wife."
    ]

-- ====================================================================
-- Optional

data Optional a = Nada | Only a deriving (Eq, Show)

newtype First' a = First' {getFirst' :: Optional a} deriving (Eq, Show)

instance Semigroup (First' a) where
  (<>) (First' Nada) x = x
  (<>) x _ = x

instance Monoid (First' a) where
  mempty = First' Nada

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    a <- arbitrary
    oneof
      [ return $ First' Nada,
        return $ First' (Only a)
      ]

instance Functor Optional where
  fmap f (Only a) = Only $ f a
  fmap _ Nada = Nada

instance Applicative Optional where
  pure = Only
  (<*>) (Only f) (Only a) = Only $ f a
  (<*>) _ _ = Nada

instance Foldable Optional where
  foldr f z (Only a) = f a z
  foldr f z Nada = z

instance Traversable Optional where
  traverse f (Only a) = Only <$> f a
  traverse f Nada = pure Nada

-- ====================================================================
-- Trivial
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

-- ====================================================================
-- Identity
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity a) (Identity b) = Identity $ a b

instance Foldable Identity where
  foldr f z (Identity a) = f a z

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

-- ====================================================================
-- Two
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two c d) = Two (a <> c) (b <> d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

-- ====================================================================
-- Two
newtype BoolConj
  = BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj a) <> (BoolConj b) = BoolConj $ a && b

instance Arbitrary BoolConj where
  arbitrary = do
    b <- arbitrary :: Gen Bool
    return $ BoolConj b

-- ====================================================================
-- Combine
newtype Combine a b = Combine {unCombine :: (a -> b)}

instance Show (Combine a b) where
  show _ = "Combine a b"

instance (Semigroup a, Semigroup b) => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine $ \a -> (g a) <> (f a)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    f <- arbitrary
    return $ Combine f

-- ====================================================================
-- Comp
newtype Comp a = Comp {unComp :: (a -> a)}

instance Show (Comp a) where
  show _ = "Comp a"

instance (Semigroup a) => Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp (f . g)

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = do
    f <- arbitrary
    return $ Comp f

-- ====================================================================
-- Validation
data Validation a b = Falla a | Exito b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (<>) (Falla a) (Falla b) = Falla $ a <> b
  (<>) (Exito a) _ = Exito a
  (<>) _ x = x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ Falla a, return $ Exito b]

-- ====================================================================
-- TalkToMe
data TalkToMe a
  = Halt
  | Print String a
  | Read (String -> a)

instance Show (TalkToMe a) where
  show _ = "Talk to me"

instance Eq (TalkToMe a) where
  (==) Halt Halt = True
  (==) (Print s1 _) (Print s2 _) = s1 == s2
  (==) (Read _) (Read _) = True

instance Arbitrary a => Arbitrary (TalkToMe a) where
  arbitrary = do
    a <- arbitrary
    s <- arbitrary :: Gen String
    r <- arbitrary :: Arbitrary a => Gen (Fun String a)
    oneof $ fmap return [Halt, Print s a, Read $ applyFun r]

instance Functor TalkToMe where
  fmap f (Halt) = Halt
  fmap f (Print s a) = Print s $ f a
  fmap f (Read g) = Read (f . g)

-- ====================================================================
-- Constant
newtype Constant a b = Constant {getConstant :: a} deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  (<*>) (Constant a1) (Constant a2) = Constant (a1 <> a2)

instance Foldable (Constant a) where
  foldr _ z _ = z

instance Traversable (Constant a) where
  traverse _ (Constant x) = pure $ Constant x

-- ====================================================================
-- List
data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Semigroup (List a) where
  (<>) (Cons x Nil) y = Cons x y
  (<>) (Cons x xs) y = Cons x (xs <> y)
  (<>) _ _ = Nil

instance Functor List where
  fmap f (Cons x xs) = Cons (f x) $ fmap f xs
  fmap _ Nil = Nil

instance Applicative List where
  pure a = Cons a Nil
  (<*>) (Cons f fs) z@(Cons y ys) = (fmap f z) <> (fs <*> ys)
  (<*>) _ _ = Nil

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    -- l <- arbitrary :: Arbitrary b => Gen (List b)
    oneof $ fmap return [Nil, Cons a Nil]

instance Foldable List where
  foldr f z Nil = z
  foldr f z (Cons x xs) = foldr f (f x z) xs

-- ====================================================================
-- ZipList
newtype ZipList' a
  = ZipList' (List a)
  deriving (Eq, Show)

instance Semigroup (ZipList' a) where
  (<>) (ZipList' a) (ZipList' b) = ZipList' $ a <> b

instance Functor ZipList' where
  fmap f (ZipList' x) = ZipList' $ fmap f x

instance Applicative ZipList' where
  pure a = ZipList' $ pure a
  (<*>) (ZipList' a) (ZipList' b) = ZipList' $ a <*> b

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = do
    a <- arbitrary
    return $ ZipList' a

-- ====================================================================
-- Either
data Either' a b
  = Left' a
  | Right' b
  deriving (Eq, Show)

instance Functor (Either' a) where
  fmap f (Right' b) = Right' $ f b
  fmap _ (Left' a) = Left' a

instance Applicative (Either' a) where
  pure x = Right' x
  (<*>) (Right' x) (Right' y) = Right' $ x y
  (<*>) (Left' x) _ = Left' x

instance Monad (Either' a) where
  (>>=) (Right' x) f = f x
  (>>=) (Left' x) _ = Left' x

-- ====================================================================
-- Big
data Big a b = Big a b b

instance (Monoid a, Monoid b) => Semigroup (Big a b) where
  (<>) (Big a b c) (Big x y z) = Big (a <> x) (b <> y) (c <> z)

instance Functor (Big a) where
  fmap f (Big a b c) = Big a (f b) (f c)

instance Foldable (Big a) where
  foldMap f x = b' <> c'
    where
      (Big a b' c') = fmap f x

instance Traversable (Big a) where
  sequenceA (Big a b c) = Big a <$> b <*> c

-- ====================================================================
-- S
data S n a = S (n a) a deriving (Eq, Show)

instance (Functor n) => Functor (S n) where
  fmap f (S na a) = S (fmap f na) (f a)

instance (Foldable n) => Foldable (S n) where
  foldr f z (S na a) = f a (foldr f z na)

instance (Traversable n) => Traversable (S n) where
  sequenceA (S na a) = S <$> (sequenceA na) <*> a

-- ====================================================================
-- Tree

data Tree a
  = Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap f Empty = Empty
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Node a b c) = Node (fmap f a) (f b) (fmap f c)

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node a b c) = (foldMap f a) <> (f b) <> (foldMap f c)

instance Traversable Tree where
  sequenceA Empty = pure Empty
  sequenceA (Leaf a) = Leaf <$> a
  sequenceA (Node a b c) = Node <$> (sequenceA a) <*> b <*> (sequenceA c)

-- ====================================================================
-- Reader
data Reader r a = Reader {runReader :: r -> a}

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ f . ra

instance Applicative (Reader r) where
  pure a = Reader $ const a
  (<*>) (Reader rf) (Reader rb) = Reader $ rf <*> rb

instance Monad (Reader r) where
  (>>=) (Reader ra) aRb = Reader (\r -> runReader (aRb (ra r)) r)

-- ====================================================================
-- Moi
data Moi s a = Moi {runState :: s -> (a, s)}

instance Functor (Moi s) where
  fmap f (Moi sa) = Moi $ (Bi.first f) . sa

instance Applicative (Moi s) where
  pure a = Moi $ \s -> (a, s)
  (<*>) (Moi sf) (Moi sa) = Moi $ Ap.liftA2 (,) (fst . sf <*> fst . sa) id

instance Monad (Moi s) where
  (>>=) (Moi sa) f = Moi $ (\s -> runState ((f . fst . sa) s) s)