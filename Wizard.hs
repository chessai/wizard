{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeOperators       #-}

module Wizard
  ( WizardT(..)
  , empty
  , singleton
  , mapWizardT
  , essence
  , leviosa
  , summon
  , foldWizardT
  
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
import Control.Monad (MonadPlus(mzero), join)
import Control.Monad.Fail (MonadFail(fail))
import Control.Monad.Fix (MonadFix(mfix))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.ST (ST)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.Coerce (coerce)
import Data.Complex (Complex)
import Data.Data (Data, Typeable)
import Data.Foldable (Foldable(foldMap))
import Data.Function (fix)
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

import qualified Data.Functor.Product as Product (Product)
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
  fmap :: forall a b f. Functor f => (a -> b) -> WizardT f a -> WizardT f b 
  fmap f = WizardT . fmap (fmap f) . wand

instance (Applicative f) => Applicative (WizardT f) where
  pure :: forall a b f. Applicative f => a -> WizardT f a 
  pure = WizardT . pure . pure
  (<*>) :: forall a b f. Applicative f => WizardT f (a -> b) -> WizardT f a -> WizardT f b 
  WizardT f <*> WizardT x = WizardT (liftA2 (<*>) f x)
  liftA2 f (WizardT x) (WizardT y) =
    WizardT (liftA2 (liftA2 f) x y)

instance (Monad m) => Monad (WizardT m) where
  (>>=) :: forall a b m. Monad m => WizardT m a -> (a -> WizardT m b) -> WizardT m b
  (>>=) = flip summon . essence

instance (Applicative f, Semigroup a) => Semigroup (WizardT f a) where
  (<>) :: WizardT f a -> WizardT f a -> WizardT f a 
  (<>) = liftA2 (<>)

instance (Applicative f, Monoid a) => Monoid (WizardT f a) where
  mempty :: WizardT f a 
  mempty = pure mempty
#if !(MIN_VERSION_base(4,11,0))
  mappend :: WizardT f a -> WizardT f a -> WizardT f a 
  mappend = liftA2 mappend
#endif

instance (Foldable f) => Foldable (WizardT f) where
  foldMap f (WizardT t) = foldMap (foldMap f) t

instance (Traversable t) => Traversable (WizardT t) where
  traverse f (WizardT t) = WizardT <$> traverse (traverse f) t

instance (Alternative f) => Alternative (WizardT f) where
  empty = WizardT Alternative.empty
  (<|>) = coerce ((<|>) :: f (f a) -> f (f a) -> f (f a))
    :: forall a . WizardT f a -> WizardT f a -> WizardT f a

instance (Alternative m, Monad m) => MonadPlus (WizardT m) where

instance (Alternative m, Monad m) => MonadFail (WizardT m) where
  fail _ = mzero

instance (MonadFix m) => MonadFix (WizardT m) where
  mfix f = WizardT (pure (mfix (essence . f)))

instance MonadTrans WizardT where
  lift = WizardT . fmap pure

instance (MonadIO m) => MonadIO (WizardT m) where
  liftIO = lift . liftIO

empty :: (Applicative f, Monoid a) => WizardT f a
empty = pure mempty
{-# INLINE empty #-}

singleton :: (Applicative f) => a -> WizardT f a
singleton = pure 
{-# INLINE singleton #-}

-- | Map over a WizardT.
mapWizardT :: Functor f => (a -> b) -> WizardT f a -> WizardT f b
mapWizardT f = WizardT . fmap (fmap f) . wand
{-# INLINE mapWizardT #-}

-- | Get the input (essence) out of the WizardT.
essence :: (Monad m) => WizardT m a -> m a
essence w = (wand w) >>= id
{-# INLINE essence #-}

-- | Lift an input into a WizardT.
leviosa :: (Monad m) => m a -> WizardT m a
leviosa = WizardT . pure
{-# INLINE leviosa #-}

-- | Summon a WizardT out of a monad.
--
-- @ '(>>=)' = 'flip' 'summon' '.' 'essence' @
summon :: Monad m => (a -> WizardT m b) -> m a -> WizardT m b
summon f = WizardT . (wand . f =<<)
{-# INLINE summon #-}

-- | Run an action over a collection of inputs.
foldWizardT
  :: forall m t a b. (Monad m, Foldable t, Monoid a, Monoid b)
  => (a -> WizardT m b)
  -> t a
  -> m b
foldWizardT f t = essence (foldMap f t)
{-# INLINE foldWizardT #-}

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
