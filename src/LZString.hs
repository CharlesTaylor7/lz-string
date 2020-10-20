{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
module LZString
  -- (decompressBase64)
  where

import Data.Function ((&), on)
import Data.Traversable (for)
import Data.Foldable (foldlM)
import Data.Word (Word8, Word16)
import Data.Bits (Bits(..))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Control.Applicative (liftA2)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (MonadState, StateT, get, put, modify, evalStateT, lift)
import Control.Monad.Except (MonadError, ExceptT, runExceptT, throwError)
import Control.Monad.Writer (MonadWriter, WriterT, execWriterT)

import System.IO.Unsafe (unsafePerformIO)

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


decompressBase64 :: ByteString -> ByteString
decompressBase64 input = _decompress(BS.length input, 32, getNextValue)
  where
    toChar :: Word8 -> Char
    toChar = toEnum . fromIntegral

    getNextValue :: Int -> Int
    getNextValue = getBaseValue . toChar . (BS.index input)


type Length = Int
type ResetValue = Int
type GetNextValue = Int -> Int


data DataStruct = DataStruct
  { _val :: Int
  , _position :: Int
  , _index :: Int
  }


_decompress :: (Length, ResetValue, GetNextValue) -> ByteString
_decompress args = unsafePerformIO $ _decompressImpl args
{-# NOINLINE _decompress #-}

newRef = lift . newIORef

while :: Monad m => m Bool -> m ()
while act = do
  result <- act
  case result of
    False -> pure ()
    True -> while act

loopMaybe :: Monad m => m (Maybe a) -> m a
loopMaybe act = do
  result <- act
  case result of
    Just e -> pure e
    Nothing -> loopMaybe act

loopM :: Monad m => (a -> ExceptT e m a) -> a -> m e
loopM act x = do
  result <- runExceptT $ act x
  case result of
    Left e -> pure e
    Right a -> loopM act a

_decompressImpl :: (Length, ResetValue, GetNextValue) -> IO ByteString
_decompressImpl (length, resetValue, getNextValue) =
  let
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
      truncated = fromIntegral c
      w = BS.singleton truncated

    if c > fromIntegral (maxBound :: Word8)
    then liftIO $ putStrLn $ "truncating " <> show c <> " to " <> show truncated
    else pure ()

    -- refs
    dictionaryRef <- newRef $ Map.fromList
      [ (0, BS.singleton $ toEnum 0)
      , (1, BS.singleton $ toEnum 1)
      , (2, BS.singleton $ toEnum 2)
      , (3, w)
      ]
    enlargeInRef <- newRef (4 :: Int)
    dictSizeRef <- newRef (4 :: Int)
    numBitsRef <- newRef (3 :: Int)
    wRef <- newRef (w :: ByteString)
    iRef <- newRef (0 :: Int)
    cRef <- newRef (0 :: Int)
    entryRef <- newRef ("" :: ByteString)

    -- loop
    result <- execWriterT $ while $
      pure False

    -- prepend initial word & return
    pure $ w <> result


type Exponent = Int

getBits :: forall m. MonadState DataStruct m => ResetValue -> GetNextValue -> Exponent -> m Word16
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
