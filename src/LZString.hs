{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module LZString (decompressBase64) where

import Prelude hiding (break, print, putStrLn)
import GHC.Exts (IsList, fromList)

import Data.Function ((&))
import Data.Foldable (foldlM, toList)
import Data.Word (Word8)
import Data.Bits (Bits(..))
import Data.IORef (IORef)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (MonadState, get, gets, put, evalStateT)
import Control.Monad.Writer.Strict (MonadWriter, execWriterT, tell)

import System.IO.Unsafe (unsafePerformIO)

import Data.ByteString.Builder
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy

-- Use an Intmap to mimic a js array
import Data.IntMap (IntMap)
import qualified Data.IntMap as Map

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Data.Array (Array)
import qualified Data.Array as Array

import Utils


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


decompressBase64 :: Strict.ByteString -> Decompressed
decompressBase64 input = _decompress(Strict.length input, 32, getNextValue)
  where
    toChar :: Word8 -> Char
    toChar = toEnum . fromIntegral

    getNextValue :: Int -> Int
    getNextValue = getBaseValue . toChar . (Strict.index input)


type InputLength = Int
type ResetValue = Int
type GetNextValue = Int -> Int
type Decompressed = Lazy.ByteString
type EntryType = String
type Dictionary = Seq EntryType

data DataStruct = DataStruct
  { _val :: Int
  , _position :: Int
  , _index :: Int
  }

_decompress :: (InputLength, ResetValue, GetNextValue) -> Decompressed
_decompress args = toLazyByteString $ unsafePerformIO $ _decompressImpl args
{-# NOINLINE _decompress #-}

f :: Int -> EntryType
f = pure . toEnum

toBytes :: EntryType -> Builder
toBytes = stringUtf8

pushBytes :: MonadWriter Builder m => EntryType -> m ()
pushBytes = tell . toBytes

unsafeHead :: EntryType -> Char
unsafeHead = head
-- unsafeHead (h Seq.:<| _) = h

snoc :: EntryType -> Char -> EntryType
snoc word c = word <> [c]

push :: EntryType -> Dictionary -> Dictionary
push = (Seq.<|)

_decompressImpl :: (InputLength, ResetValue, GetNextValue) -> IO Builder
_decompressImpl (inputLength, resetValue, getNextValue) =
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
          n -> error $ "bits == " <> show n
    bits <- getBits' exponent
    let w = f bits
    -- refs
    dictionaryRef <- newRef $
      (fromList [ f 0, f 1, f 2, w ] :: Dictionary)
    enlargeInRef <- newRef (4 :: Int)
    numBitsRef <- newRef (3 :: Int)
    wRef <- newRef (w :: EntryType)
    cRef <- newRef undefined

    -- loop
    result <- execWriterT $ loop $ do
      pushBytes w

      data_index <- gets _index
      when (data_index > inputLength) $
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
        modifyRef dictionaryRef $ push (f bits)
        dictSize <- length <$> readRef dictionaryRef

        -- Line 457 in lz-string.js
        writeRef cRef $ dictSize - 1

        tickEnlargeIn enlargeInRef numBitsRef

      -- line 469
      c <- readRef cRef
      dictionary <- readRef dictionaryRef
      let dictSize = length dictionary
      w <- readRef wRef
      entry <- do
        case dictionary Seq.!? c of
          Just val -> do
            pure val
          Nothing ->
            if c == dictSize
            then do
              pure $ w `snoc` (unsafeHead w)
            else
              error "return null"

      pushBytes entry

      modifyRef dictionaryRef $
        push (w `snoc` (unsafeHead entry))

      writeRef wRef entry
      tickEnlargeIn enlargeInRef numBitsRef

    -- prepend initial word & return
    pure $ result


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
