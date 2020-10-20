{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module LZString (decompressBase64) where

import Data.Function ((&), on)
import Data.Traversable (for)
import Data.Foldable (foldlM)
import Data.Word (Word8)
import Data.Bits (Bits(..))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Control.Applicative (liftA2)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (MonadState, State(..), get, put, modify, evalStateT, lift)

import System.IO.Unsafe (unsafePerformIO)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

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

f :: Int -> Char
f = toEnum

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

_decompressImpl :: (Length, ResetValue, GetNextValue) -> IO ByteString
_decompressImpl (length, resetValue, getNextValue) =
  let
    dataStruct = DataStruct
      { _val = getNextValue 0
      , _position = resetValue
      , _index = 1
      }

  in flip evalStateT dataStruct $ do
    dictionaryRef <- newRef (mempty :: IntMap ByteString)
    enlargeInRef <- newRef (4 :: Int)
    dictSizeRef <- newRef (4 :: Int)
    numBitsRef <- newRef (3 :: Int)
    iRef <- newRef (0 :: Int)
    cRef <- newRef (0 :: Int)
    entryRef <- newRef ("" :: ByteString)
    wRef <- newRef (Nothing :: Maybe ByteString)
    resultRef <- newRef ("" :: ByteString)

    liftIO $ for [0..2] $ \i -> do
      modifyIORef' dictionaryRef $
        Map.insert i $ BS.singleton (toEnum i)

    bits <- getBits resetValue getNextValue 2

    pure undefined


type Exponent = Int
type BitPattern = Int

getBits :: forall m. MonadState DataStruct m => ResetValue -> GetNextValue -> Exponent -> m BitPattern
getBits resetValue getNextValue max = foldlM reducer 0 [0..(max-1)]
  where
    reducer :: BitPattern -> Exponent -> m BitPattern
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
        power = 1 `shiftL` n :: Int
        resultBit = if val .&. pos > 0 then 1 else 0

      pure $ bits .|. resultBit * power
