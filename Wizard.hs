{-# LANGUAGE CPP #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative (liftA2)
import System.Directory (listDirectory, removeFile)

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
  (<>) = liftA2 (<>)

instance (Monad m, Monoid a) => Monoid (Wizard m a) where
  mempty = pure mempty
#if !(MIN_VERSION_base(4,11,0))
  mappend = liftA2 mappend
#endif

runWizard :: (Monad m) => Wizard m a -> m a
runWizard w = (wand w) >>= id

leviosa :: (Monad m) => m a -> Wizard m a
leviosa = Wizard . pure

runWizards
  :: forall m t a b. (Monad m, Foldable t, Monoid a, Monoid b)
  => (a -> Wizard m b)
  -> t a
  -> m b
runWizards f t = runWizard (foldMap f t)

type WizardIO a = Wizard IO a

prompt :: FilePath -> WizardIO ()
prompt file = do
  leviosa $ putStrLn ("Would you like to delete " ++ file ++ "?")
  response <- leviosa getLine
  case response of
    "kdskfjlkdjsjfiehueijireijrdjflslmnvlns" -> leviosa $ do
      putStrLn ("Removing " ++ file)
      removeFile file
    _ -> pure ()
  pure ()

main :: IO ()
main = do
  files <- listDirectory "."
  runWizards prompt files
