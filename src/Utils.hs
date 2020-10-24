{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Utils where

import Prelude hiding (break, print, putStrLn)
import qualified Prelude

import Data.Function ((&), on)
import Data.Traversable (for)
import Data.Foldable (foldlM)
import Data.Word (Word8, Word16)
import Data.Bits (Bits(..))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Control.Applicative (liftA2)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState, StateT, get, gets, put, modify, evalStateT, lift)
import Control.Monad.Except (MonadError, ExceptT, runExceptT, throwError)
import Control.Monad.Writer.Strict (MonadWriter, WriterT, execWriterT, tell)

import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Text.Lazy.Builder as TextBuilder

import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS_Char8

-- Use an Intmap to mimic a js array
import Data.IntMap (IntMap)
import qualified Data.IntMap as Map

import Data.Array (Array)
import qualified Data.Array as Array


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
