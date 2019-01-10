{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -Wall #-}

module Wizard
  ( WizardT(..)
  
    -- * Fantastical use of a Wizard
  , egg
  , tadpole
  
  , transform
  , essence
  , leviosa
  , summon
  , sageMode
 
    -- * Boring use of a Wizard
  , empty
  , singleton
  , mapWizard
  , foldWizard
  ) where

import Control.Applicative (liftA2, Alternative((<|>)))
import Control.Monad (MonadPlus(mzero))
import Control.Monad.Fail (MonadFail(fail))
import Control.Monad.Fix (MonadFix(mfix))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.Coerce (coerce)
import Data.Data (Typeable)
import Data.Foldable (Foldable(foldMap))
import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup((<>)))
import Data.Traversable (Traversable(traverse))
import GHC.Generics (Generic, Generic1)

import qualified Control.Applicative as Alternative (empty)

infixr 9 `WizardT`

-- | A 'Wizard' Monoid, based on ideas expressed by Gabriel Gonzalez
-- at http://www.haskellforall.com/2018/02/the-wizard-monoid.html.
--
-- One can view this as 'Data.Functor.Compose.Compose', specialised
-- to a single functor.
newtype WizardT m a = WizardT { wand :: m (m a) }
  deriving (Generic, Generic1, Typeable)

instance (Functor f) => Functor (WizardT f) where
  fmap f = WizardT . fmap (fmap f) . wand
  {-# INLINE fmap #-}

instance (Applicative f) => Applicative (WizardT f) where
  pure = WizardT . pure . pure
  {-# INLINE pure #-} 
  WizardT f <*> WizardT x = WizardT (liftA2 (<*>) f x)
  liftA2 f (WizardT x) (WizardT y) =
    WizardT (liftA2 (liftA2 f) x y)
  {-# INLINE (<*>) #-}

instance (Monad m) => Monad (WizardT m) where
  (>>=) = flip summon . essence
  {-# INLINE (>>=) #-}

instance (Applicative f, Semigroup a) => Semigroup (WizardT f a) where
  (<>) :: WizardT f a -> WizardT f a -> WizardT f a 
  (<>) = liftA2 (<>)
  {-# INLINE (<>) #-}

instance (Applicative f, Monoid a) => Monoid (WizardT f a) where
  mempty :: WizardT f a 
  mempty = pure mempty
  {-# INLINE mempty #-}
#if !(MIN_VERSION_base(4,11,0))
  mappend :: WizardT f a -> WizardT f a -> WizardT f a 
  mappend = liftA2 mappend
  {-# INLINE mappend #-}
#endif

instance (Foldable f) => Foldable (WizardT f) where
  foldMap f (WizardT t) = foldMap (foldMap f) t
  {-# INLINE foldMap #-}

instance (Traversable t) => Traversable (WizardT t) where
  traverse f (WizardT t) = WizardT <$> traverse (traverse f) t
  {-# INLINE traverse #-}

instance (Alternative f) => Alternative (WizardT f) where
  empty = WizardT Alternative.empty
  {-# INLINE empty #-} 
  (<|>) = coerce ((<|>) :: f (f a) -> f (f a) -> f (f a))
    :: forall a . WizardT f a -> WizardT f a -> WizardT f a
  {-# INLINE (<|>) #-}

instance (Alternative m, Monad m) => MonadPlus (WizardT m) where

instance (Alternative m, Monad m) => MonadFail (WizardT m) where
  fail _ = mzero

instance (MonadFix m) => MonadFix (WizardT m) where
  mfix f = WizardT (pure (mfix (essence . f)))

instance MonadTrans WizardT where
  lift = WizardT . fmap pure
  {-# INLINE lift #-}

instance (MonadIO m) => MonadIO (WizardT m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}

-- | /O(1)/ boring - construct an empty Wizard.
empty :: (Applicative f, Monoid a) => WizardT f a
empty = egg
{-# INLINE empty #-}

-- | /O(1)/ boring - construct a singleton Wizard.
singleton :: (Applicative f) => a -> WizardT f a
singleton = pure 
{-# INLINE singleton #-}

-- | /O(1)/ fantastical - construct an empty Wizard.
egg :: (Applicative f, Monoid a) => WizardT f a
egg = pure mempty
{-# INLINE egg #-}

-- | /O(1)/ fantastical - construct a singleton Wizard.
tadpole :: (Applicative f) => a -> WizardT f a
tadpole = pure 
{-# INLINE tadpole #-}

-- | Map over a Wizard in a fantastical manner.
transform :: Functor f => (a -> b) -> WizardT f a -> WizardT f b
transform f = WizardT . fmap (fmap f) . wand
{-# INLINE transform #-}

-- | Map over a Wizard in a boring manner.
mapWizard :: Functor f => (a -> b) -> WizardT f a -> WizardT f b
mapWizard f = WizardT . fmap (fmap f) . wand
{-# INLINE mapWizard #-}

-- | Get the input (essence) out of the Wizard.
essence :: (Monad m) => WizardT m a -> m a
essence w = (wand w) >>= id
{-# INLINE essence #-}

-- | Lift an input into a Wizard.
leviosa :: (Monad m) => m a -> WizardT m a
leviosa = WizardT . pure
{-# INLINE leviosa #-}

-- | Summon a Wizard out of a monad using
--   a conjuring spell.
--
--   @ ('>>=') = 'flip' 'summon' '.' 'essence' @
summon :: Monad m => (a -> WizardT m b) -> m a -> WizardT m b
summon f = WizardT . (wand . f =<<)
{-# INLINE summon #-}

-- | Run an action over a collection of inputs
--   fantastically.
--
--   This is like 'Beast Mode', but specialised
--   to 'Wizard's.
sageMode
  :: forall m t a b. (Monad m, Foldable t, Monoid a, Monoid b)
  => (a -> WizardT m b)
  -> t a
  -> m b
sageMode f t = essence (foldMap f t)
{-# INLINE sageMode #-}

-- | Run an action over a collection of inputs.
foldWizard
  :: forall m t a b. (Monad m, Foldable t, Monoid a, Monoid b)
  => (a -> WizardT m b)
  -> t a
  -> m b
foldWizard f t = essence (foldMap f t)
{-# INLINE foldWizard #-}
