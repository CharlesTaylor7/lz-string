{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
module LZString
  -- (decompressBase64)
  where

import Prelude hiding (break)
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
import Debug.Trace (traceId)

import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS_Char8

-- Use an Intmap to mimic a js array
import Data.IntMap (IntMap)
import qualified Data.IntMap as Map

import Data.Array (Array)
import qualified Data.Array as Array


keyStrBase64 :: Array Int Char
keyStrBase64 = Array.listArray (0,63) "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/="

reverseLookup :: IntMap Int
reverseLookup =
  Array.assocs keyStrBase64
  & map (\(i, c) -> (fromEnum c, i))
  & Map.fromList

-- | Translate character to its ordinal position in the keyStrBase64
getBaseValue :: Char -> Int
getBaseValue = (reverseLookup Map.!) . fromEnum


decompressBase64 :: ByteString -> Decompressed
decompressBase64 input = _decompress(BS.length input, 32, getNextValue)
  where
    toChar :: Word8 -> Char
    toChar = toEnum . fromIntegral

    getNextValue :: Int -> Int
    getNextValue = getBaseValue . toChar . (BS.index input)


type Length = Int
type ResetValue = Int
type GetNextValue = Int -> Int
type Decompressed = String

data DataStruct = DataStruct
  { _val :: Int
  , _position :: Int
  , _index :: Int
  }


_decompress :: (Length, ResetValue, GetNextValue) -> Decompressed
_decompress args = unsafePerformIO $ _decompressImpl args
{-# NOINLINE _decompress #-}

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

f :: Int -> String
f = traceId . pure . toEnum

_decompressImpl :: (Length, ResetValue, GetNextValue) -> IO Decompressed
_decompressImpl (length, resetValue, getNextValue) =
  let
    getBits' :: MonadState DataStruct m => Int -> m Int
    getBits' = getBits resetValue getNextValue

    dataStruct = DataStruct
      { _val = getNextValue 0
      , _position = resetValue
      , _index = 1
      }
  in flip evalStateT dataStruct $ do

    bits <- getBits' 2

    let
      exponent =
        case bits of
          0 -> 8
          1 -> 16
          2 -> error "bits == 2"
    c <- getBits' exponent
    let
      w = f c

    -- refs
    dictionaryRef <- newRef $ Map.fromList
      [ (0, f 0)
      , (1, f 1)
      , (2, f 2)
      , (3, w)
      ]
    enlargeInRef <- newRef (4 :: Int)
    dictSizeRef <- newRef (4 :: Int)
    numBitsRef <- newRef (3 :: Int)
    wRef <- newRef (w :: Decompressed)
    iRef <- newRef (0 :: Int)
    cRef <- newRef c
    entryRef <- newRef ("" :: Decompressed)

    -- loop
    result <- execWriterT $ loop $ do
      data_index <- gets _index
      when (data_index > length) $
        break

      numBits <- readRef numBitsRef
      bits <- getBits' numBits

      -- assignment in switch case of line 423
      writeRef cRef bits

      -- switch case 2
      when (bits == 2) $
        break

      -- switch case 0, 1
      -- line 423
      when (bits < 2) $ do
        let exponent = (bits + 1) * 8

        bits <- getBits' exponent
        dictSize <- incrementRef dictSizeRef
        modifyRef dictionaryRef $
          Map.insert dictSize (f bits)

        -- Line 457 in lz-string.js
        writeRef cRef $ dictSize - 1

        tickEnlargeIn enlargeInRef numBitsRef

      -- line 469
      c <- readRef cRef
      dictSize <- readRef dictSizeRef
      dictionary <- readRef dictionaryRef
      w <- readRef wRef
      let
        entry =
          case dictionary Map.!? c of
            Just val -> val
            Nothing ->
              if c == dictSize
              then w <> [(w !! 0)]
              else
                error "return null"

      tell entry

      dictSize <- incrementRef dictSizeRef
      modifyRef dictionaryRef $
        Map.insert dictSize (w <> [entry !! 0])

      writeRef wRef entry
      tickEnlargeIn enlargeInRef numBitsRef

    -- prepend initial word & return
    pure $ w <> result


tickEnlargeIn :: MonadIO m => IORef Int -> IORef Int -> m ()
tickEnlargeIn enlargeInRef numBitsRef = do
  enlargeIn <- decrementRef enlargeInRef

  when (enlargeIn == 1) $ do
    numBits <- incrementRef numBitsRef
    writeRef enlargeInRef $ (1 `shiftL` numBits)


type Exponent = Int

getBits :: forall m. MonadState DataStruct m => ResetValue -> GetNextValue -> Exponent -> m Int
getBits resetValue getNextValue max = foldlM reducer 0 [0..(max-1)]
  where
    reducer bits n = do

    -- read current values
      DataStruct val pos index <- get

      -- advance enumerator
      let pos' = pos `shiftR` 1
      put $
        if pos' /= 0
        then DataStruct val pos' index
        else DataStruct (getNextValue index) resetValue (index + 1)


      -- apply new bit
      let
        power = 1 `shiftL` n
        resultBit = if val .&. pos > 0 then 1 else 0

      pure $ bits .|. resultBit * power
