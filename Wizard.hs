{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

import Control.Applicative (liftA2, Alternative((<|>)))
import Control.Monad (MonadPlus(mzero))
import Control.Monad.Fail (MonadFail(fail))
import Control.Monad.Fix (MonadFix(mfix))
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Coerce (coerce)
import Data.Data (Data, Typeable)
import Data.Foldable (Foldable(foldMap))
import Data.Function (fix)
import Data.Monoid ((<>))
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup(..))
#endif
import Data.Traversable (Traversable(traverse))
import GHC.Generics (Generic, Generic1)

import qualified Control.Applicative as Alternative (empty)

infixr 9 `Wizard`

-- | A 'Wizard' Monoid, based on ideas expressed by Gabriel Gonzalez
-- at http://www.haskellforall.com/2018/02/the-wizard-monoid.html.
--
-- One can view this as 'Data.Functor.Compose.Compose', specialised
-- to a single functor.
newtype Wizard m a = Wizard { wand :: m (m a) }
  deriving (Generic, Generic1, Typeable)

empty :: (Applicative f, Monoid a) => Wizard f a
empty = pure mempty

singleton :: (Applicative f) => a -> Wizard f a
singleton = pure 

instance (Functor f) => Functor (Wizard f) where
  fmap :: forall a b f. Functor f => (a -> b) -> Wizard f a -> Wizard f b 
  fmap f = Wizard . fmap (fmap f) . wand

instance (Applicative f) => Applicative (Wizard f) where
  pure :: forall a b f. Applicative f => a -> Wizard f a 
  pure = Wizard . pure . pure
  (<*>) :: forall a b f. Applicative f => Wizard f (a -> b) -> Wizard f a -> Wizard f b 
  Wizard f <*> Wizard x = Wizard (liftA2 (<*>) f x)
  liftA2 f (Wizard x) (Wizard y) =
    Wizard (liftA2 (liftA2 f) x y)

instance (Monad m) => Monad (Wizard m) where
  (>>=) :: forall a b m. Monad m => Wizard m a -> (a -> Wizard m b) -> Wizard m b
  (>>=) = flip summon . essence

instance (Applicative f, Semigroup a) => Semigroup (Wizard f a) where
  (<>) :: Wizard f a -> Wizard f a -> Wizard f a 
  (<>) = liftA2 (<>)

instance (Applicative f, Monoid a) => Monoid (Wizard f a) where
  mempty :: Wizard f a 
  mempty = pure mempty
#if !(MIN_VERSION_base(4,11,0))
  mappend :: Wizard f a -> Wizard f a -> Wizard f a 
  mappend = liftA2 mappend
#endif

instance (Foldable f) => Foldable (Wizard f) where
  foldMap f (Wizard t) = foldMap (foldMap f) t

instance (Traversable t) => Traversable (Wizard t) where
  traverse f (Wizard t) = Wizard <$> traverse (traverse f) t

instance (Alternative f) => Alternative (Wizard f) where
  empty = Wizard Alternative.empty
  (<|>) = coerce ((<|>) :: f (f a) -> f (f a) -> f (f a))
    :: forall a . Wizard f a -> Wizard f a -> Wizard f a

instance (Alternative m, Monad m) => MonadPlus (Wizard m) where

instance (Alternative m, Monad m) => MonadFail (Wizard m) where
  fail _ = mzero

instance (MonadFix m) => MonadFix (Wizard m) where
  mfix = mfix pure

-- | Map over a Wizard.
mapWizard :: Functor f => (a -> b) -> Wizard f a -> Wizard f b
mapWizard f = Wizard . fmap (fmap f) . wand

-- | Get the input (essence) out of the Wizard.
essence :: (Monad m) => Wizard m a -> m a
essence w = (wand w) >>= id

-- | Lift an input into a Wizard.
leviosa :: (Monad m) => m a -> Wizard m a
leviosa = Wizard . pure

-- | Summon a Wizard out of a monad.
--
-- @ '(>>=)' = 'flip' 'summon' '.' 'essence' @
summon :: Monad m => (a -> Wizard m b) -> m a -> Wizard m b
summon f = Wizard . (wand . f =<<)

-- | Run an action over a collection of inputs.
foldWizard
  :: forall m t a b. (Monad m, Foldable t, Monoid a, Monoid b)
  => (a -> Wizard m b)
  -> t a
  -> m b
foldWizard f t = essence (foldMap f t)

instance MonadTrans Wizard where
  lift = Wizard . fmap pure

instance (MonadIO m) => MonadIO (Wizard m) where
  liftIO = lift . liftIO
