{-# LANGUAGE CPP #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative (liftA2)
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup(..))
#endif
import Data.Monoid ((<>))

-- | A 'Wizard' Monoid, based on ideas expressed by Gabriel Gonzalez
-- at http://www.haskellforall.com/2018/02/the-wizard-monoid.html.
newtype Wizard m a = Wizard { wand :: m (m a) }

instance (Functor m) => Functor (Wizard m) where
  fmap :: forall a b m. Functor m => (a -> b) -> Wizard m a -> Wizard m b 
  fmap f = Wizard . fmap (fmap f) . wand

instance (Monad m) => Applicative (Wizard m) where
  pure :: forall a b m. Monad m => a -> Wizard m a 
  pure = Wizard . pure . pure
  (<*>) :: forall a b m. Monad m => Wizard m (a -> b) -> Wizard m a -> Wizard m b 
  k <*> a = Wizard $
    (wand k >>= id) >>= (\f -> fmap (fmap f) (wand a))

instance (Monad m) => Monad (Wizard m) where
  (>>=) :: forall a b m. Monad m => Wizard m a -> (a -> Wizard m b) -> Wizard m b
  k >>= f = Wizard $
    id =<< (fmap (wand . f) $ (wand k >>= id))

instance (Monad m, Semigroup a) => Semigroup (Wizard m a) where
  (<>) :: Wizard m a -> Wizard m a -> Wizard m a 
  (<>) = liftA2 (<>)

instance (Monad m, Monoid a) => Monoid (Wizard m a) where
  mempty :: Wizard m a 
  mempty = pure mempty
#if !(MIN_VERSION_base(4,11,0))
  mappend :: Wizard m a -> Wizard m a -> Wizard m a 
  mappend = liftA2 mappend
#endif

-- | Map over a Wizard.
mapWizard :: Functor m => (a -> b) -> Wizard m a -> Wizard m b
mapWizard f = Wizard . fmap (fmap f) . wand

-- | Get the input out of the Wizard.
runWizard :: (Monad m) => Wizard m a -> m a
runWizard w = (wand w) >>= id

-- | Lift an input into a Wizard.
leviosa :: (Monad m) => m a -> Wizard m a
leviosa = Wizard . pure

-- | Run an action over a collection of inputs.
foldWizard
  :: forall m t a b. (Monad m, Foldable t, Monoid a, Monoid b)
  => (a -> Wizard m b)
  -> t a
  -> m b
foldWizard f t = runWizard (foldMap f t)
