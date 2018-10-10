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
    
    -- * Monomophised Wizards
  , Wizard
  , WizardAlt
  , WizardComplex
  , WizardDown
  , WizardDual
  , WizardEither
  , WizardEndo
  , WizardF
  , WizardFirst
  , WizardGProd
  , WizardIO
  , WizardLast
  , WizardList
  , WizardM1
  , WizardMax
  , WizardMaybe
  , WizardMin
  , WizardNonEmpty
  , WizardOption
  , WizardPar1
  , WizardProduct
  , WizardProxy
  , WizardReadP
  , WizardReadPrec
  , WizardRec1
  , WizardST
  , WizardSTM
  , WizardSum
  , WizardTuple
  , WizardU1
  ) where

import Control.Applicative (liftA2, Alternative((<|>)))
import Control.Monad (MonadPlus(mzero))
import Control.Monad.Fail (MonadFail(fail))
import Control.Monad.Fix (MonadFix(mfix))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.ST (ST)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.Coerce (coerce)
import Data.Complex (Complex)
import Data.Data (Typeable)
import Data.Foldable (Foldable(foldMap))
import Data.Functor.Identity (Identity)
import Data.List.NonEmpty (NonEmpty)
import Data.Monoid (Monoid(mempty,mappend), Product, Sum, Dual, Last, First, Alt)
import Data.Ord (Down)
import Data.Proxy (Proxy)
import Data.Semigroup (Semigroup((<>)), Option, Max, Min)
import Data.Traversable (Traversable(traverse))
import GHC.Conc (STM)
import GHC.Generics (Generic, Generic1, U1, Par1, Rec1, M1, (:*:))
import Text.ParserCombinators.ReadP (ReadP)
import Text.ParserCombinators.ReadPrec (ReadPrec)

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

type Wizard             a   = WizardT Identity a
type WizardEndo         a   = WizardT ((->) a) a
type WizardF            a b = WizardT ((->) b) a
type WizardIO           a   = WizardT IO a
type WizardList         a   = WizardT [] a
type WizardMaybe        a   = WizardT Maybe a
type WizardEither       a e = WizardT (Either e) a
type WizardTuple        a   = WizardT ((,) a) a
type WizardU1           a   = WizardT U1 a
type WizardPar1         a   = WizardT Par1 a
type WizardRec1       f a   = WizardT (Rec1 f) a
type WizardM1     i c f a   = WizardT (M1 i c f) a
type WizardGProd    f g a   = WizardT (f :*: g) a
type WizardNonEmpty     a   = WizardT NonEmpty a
type WizardSTM          a   = WizardT STM a
type WizardReadP        a   = WizardT ReadP a
type WizardReadPrec     a   = WizardT ReadPrec a
type WizardDown         a   = WizardT Down a
type WizardProduct      a   = WizardT Product a
type WizardSum          a   = WizardT Sum a
type WizardDual         a   = WizardT Dual a
type WizardLast         a   = WizardT Last a
type WizardFirst        a   = WizardT First a
type WizardOption       a   = WizardT Option a
type WizardMax          a   = WizardT Max a
type WizardMin          a   = WizardT Min a
type WizardComplex      a   = WizardT Complex a
type WizardST         s a   = WizardT (ST s) a
type WizardProxy        a   = WizardT Proxy a
type WizardAlt        f a   = WizardT (Alt f) a
