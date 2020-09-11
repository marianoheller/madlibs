{-# LANGUAGE RankNTypes #-}

module Lib where

import qualified Control.Applicative as Ap
import qualified Control.Monad as M
import qualified Data.Bifunctor as Bi
import Data.Foldable
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
  (<*>) (Reader rab) (Reader ra) = Reader $ rab <*> ra

instance Monad (Reader r) where
  (>>=) (Reader ra) aRb = Reader (\r -> runReader (aRb (ra r)) r)

-- ====================================================================
-- Moi
data Moi s a = Moi {runState :: s -> (a, s)}

instance Functor (Moi s) where
  fmap f (Moi sa) = Moi $ (Bi.first f) . sa

instance Applicative (Moi s) where
  pure a = Moi $ \s -> (a, s)
  (<*>) (Moi sf) (Moi sa) = Moi $ Ap.liftA2 (,) (sab' <*> sa') id
    where
      sa' = fst . sa
      sab' = fst . sf

instance Monad (Moi s) where
  (>>=) (Moi sa) f = Moi $ (\s -> runState ((f . fst . sa) s) s)

get :: Moi s s
get = Moi (\s -> (s, s))

put :: s -> Moi s ()
put s = Moi (\_ -> ((), s))

exec :: Moi s a -> s -> s
exec = (snd .) . runState

eval :: Moi s a -> s -> a
eval = (fst .) . runState

modify :: (s -> s) -> Moi s ()
modify f = Moi (\s -> ((), f s))

-- ====================================================================
-- Compose

data Compose f g a = Compose {getCompose :: f (g a)}

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure a = Compose $ pure . pure $ a
  (<*>) (Compose fgab) (Compose fga) = Compose $ Ap.liftA2 (<*>) fgab fga

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap f (Compose fga) = (foldMap . foldMap) f fga

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse afb (Compose fga) = Compose <$> (traverse . traverse) afb fga

-- ====================================================================
-- IndentityT
newtype IdentityT f a = IdentityT {runIdentityT :: f a}
  deriving (Eq, Show)

instance Functor f => Functor (IdentityT f) where
  fmap f (IdentityT fa) = IdentityT $ fmap f fa

instance Applicative f => Applicative (IdentityT f) where
  pure a = IdentityT $ pure a
  (<*>) (IdentityT f) (IdentityT fa) = IdentityT $ f <*> fa

instance Monad m => Monad (IdentityT m) where
  (IdentityT ma) >>= f = IdentityT $ ma >>= (runIdentityT . f)

-- ====================================================================
-- MaybeT
newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

instance Functor f => Functor (MaybeT f) where
  fmap f (MaybeT fa) = MaybeT $ (fmap . fmap) f fa

instance Applicative f => Applicative (MaybeT f) where
  pure a = MaybeT . pure . Just $ a
  (<*>) (MaybeT fab) (MaybeT fa) = MaybeT $ Ap.liftA2 (<*>) fab fa

instance Monad m => Monad (MaybeT m) where
  (MaybeT mma) >>= f = MaybeT $ mma >>= (runMaybeT . g)
    where
      g ma = case ma of
        Nothing -> MaybeT . pure $ Nothing
        Just a -> f a

-- ====================================================================
-- EitherT
newtype EitherT e m a = EitherT {runEitherT :: m (Either e a)}

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT ema) = EitherT $ (fmap . fmap) f ema

instance Applicative m => Applicative (EitherT e m) where
  pure a = EitherT . pure . pure $ a
  (<*>) (EitherT emf) (EitherT ema) = EitherT $ Ap.liftA2 (<*>) emf ema

instance Monad m => Monad (EitherT e m) where
  (EitherT mea) >>= f = EitherT $ do
    ea <- mea
    case ea of
      Left e -> return $ Left e
      Right a -> runEitherT $ f a

swapEither :: Either e a -> Either a e
swapEither ea = case ea of
  Right a -> Left a
  Left e -> Right e

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ema) = EitherT $ fmap swapEither ema

eitherT :: (Monad m) => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g (EitherT mb) = do
  eab <- mb
  case eab of
    Left a -> f a
    Right b -> g b

instance MonadTrans (EitherT e) where
  lift ma = EitherT $ fmap Right ma

-- ====================================================================
-- ReaderT
newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a}

instance Functor m => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance Applicative m => Applicative (ReaderT r m) where
  pure a = ReaderT . pure . pure $ a
  (<*>) (ReaderT rmf) (ReaderT rma) = ReaderT $ Ap.liftA2 (<*>) rmf rma

instance Monad m => Monad (ReaderT r m) where
  (ReaderT rma) >>= f = ReaderT $ \r -> do
    a <- rma r
    let rmb = runReaderT . f $ a
    rmb r

instance MonadTrans (ReaderT r) where
  lift = ReaderT . const

instance (MonadIO m) => MonadIO (ReaderT r m) where
  liftIO = ReaderT . const . liftIO

-- ====================================================================
-- StateT
newtype StateT s m a = StateT {runStateT :: s -> m (a, s)}

instance Functor m => Functor (StateT s m) where
  fmap f (StateT sma) = StateT $ (fmap . fmap) (Bi.first f) sma

instance Monad m => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)
  (<*>) (StateT smf) (StateT sma) = StateT $ \s -> do
    (f, s') <- smf s
    (a, s'') <- sma s'
    return (f a, s'')

instance Monad m => Monad (StateT s m) where
  (StateT sma) >>= f = StateT $ \s -> do
    a <- sma s
    let smb = runStateT . f . fst $ a
    smb s

instance MonadTrans (StateT s) where
  lift ma = StateT $ \s -> do
    a <- ma
    return (a, s)

instance (MonadIO m) => MonadIO (StateT s m) where
  liftIO ioa = StateT $ \s -> do
    a <- liftIO ioa
    return (a, s)

-- ====================================================================
-- MonadTrans
class MonadTrans t where
  lift :: Monad m => m a -> t m a

-- ====================================================================
--  MonadIO
class (Monad m) => MonadIO m where
  liftIO :: IO a -> m a

-- ====================================================================
--  Monad transformers excersices
rDec :: Num a => Reader a a
rDec = Reader $ ((-) 1)

rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ Identity . show

rPrintAndInc :: (Show a, Num a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \r -> do
  putStrLn $ "Hi: " ++ show r
  return $ r + 1

-- ====================================================================
-- Cont
newtype Cont a = Cont {unCont :: forall r. (a -> r) -> r}

cont :: a -> Cont a
cont a =
  let cb = id
   in Cont (\cb -> cb a)

instance Functor Cont where
  fmap f (Cont c) = cont $ c f
