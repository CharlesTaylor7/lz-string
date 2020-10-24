{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Utils where

import Prelude hiding (break, print, putStrLn)
import qualified Prelude

import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Except (MonadError, ExceptT, runExceptT, throwError)


print :: (Show a, MonadIO m) => a -> m ()
print = liftIO . Prelude.print

dump :: MonadIO m => String -> m ()
dump = liftIO . Prelude.putStrLn

newRef :: MonadIO m => a -> m (IORef a)
newRef = liftIO . newIORef

readRef :: MonadIO m => IORef a -> m a
readRef = liftIO . readIORef

writeRef :: MonadIO m => IORef a  -> a -> m ()
writeRef = (liftIO .) . writeIORef

modifyRef :: MonadIO m => IORef a -> (a -> a) -> m ()
modifyRef = (liftIO .) . modifyIORef'

-- | C-style language postfix ++ operator
-- increment the ref, but return the old value
incrementRef :: MonadIO m => IORef Int -> m Int
incrementRef ref = liftIO $ do
  i <- readIORef ref
  writeIORef ref $ i + 1
  pure i

-- | C-style language postfix -- operator
-- decrement the ref, but return the old value
decrementRef :: MonadIO m => IORef Int -> m Int
decrementRef ref = liftIO $ do
  i <- readIORef ref
  writeIORef ref $ i - 1
  pure i

loop :: forall e m. Monad m => ExceptT e m () -> m e
loop act = do
  result <- runExceptT act
  case result of
    Left e -> pure e
    Right _ -> loop act

break :: MonadError () m => m a
break = throwError ()
