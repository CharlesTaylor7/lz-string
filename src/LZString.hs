{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module LZString (decompressBase64) where

import Prelude hiding (break, print, putStrLn)

import GHC.Exts
import Data.Function ((&))
import Data.Foldable (foldlM)
import Data.Word (Word8)
import Data.Bits (Bits(..))
import Data.IORef (IORef)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (MonadState, get, gets, put, evalStateT)
import Control.Monad.Writer.Strict (execWriterT, tell)

import System.IO.Unsafe (unsafePerformIO)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import qualified Data.ByteString.Builder as L
import qualified Data.ByteString.Lazy as L

import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq

import Data.IntMap (IntMap)
import qualified Data.IntMap as Map

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
type Decompressed = L.ByteString
type Builder = L.Builder
type EntryType = Seq Char
type Dictionary = Seq EntryType

data DecompressionState = DecompressionState
  { _dataStruct :: !DataStruct
  , _bitCounter :: !BitCounter
  , _dictionary :: !Dictionary
  , _word       :: !EntryType
  }

data DataStruct = DataStruct
  { _value    :: !Int
  , _position :: !Int
  , _index    :: !Int
  }

data BitCounter = BitCounter
  { _enlargeIn :: Int
  , _numBits :: Int
  }

_decompress :: (Length, ResetValue, GetNextValue) -> Decompressed
_decompress args = L.toLazyByteString $ unsafePerformIO $ _decompressImpl args
{-# NOINLINE _decompress #-}

f :: Int -> Seq Char
f = pure . toEnum

unsafeHead :: Seq a -> a
unsafeHead (h :<| _) = h


_decompressImpl :: (Length, ResetValue, GetNextValue) -> IO Builder
_decompressImpl (inputLength, resetValue, getNextValue) =
  let
    getBits' :: MonadState DataStruct m => Int -> m Int
    getBits' = getBits resetValue getNextValue

    dataStruct = DataStruct
      { _value    = getNextValue 0
      , _position = resetValue
      , _index    = 1
      }
  in flip evalStateT dataStruct $ do

    bits <- getBits' 2

    let
      exponent =
        case bits of
          0 -> 8
          1 -> 16
          n -> error $ "bits == " <> show n
    c <- getBits' exponent
    let w = f c

    -- refs
    dictionaryRef <- newRef $
      (fromList [ f 0, f 1, f 2, w] :: Seq EntryType)
    bitCounterRef <- newRef (BitCounter 4 3)
    wRef <- newRef (w :: EntryType)

    execWriterT $ do
      tell $ toBytes w
      -- loop
      loop $ do
        data_index <- gets _index
        when (data_index > inputLength) $
          break

        numBits <- _numBits <$> readRef bitCounterRef
        bits <- getBits' numBits

        -- switch case 2
        when (bits == 2) $
          break

        -- switch case 0, 1
        -- line 423
        c <- if bits > 2 then pure bits else do
          modifyRef bitCounterRef tick

          let exponent = (bits + 1) * 8

          bits <- getBits' exponent
          modifyRef dictionaryRef $ (:|> (f bits))

          dictSize <- length <$> readRef dictionaryRef
          -- Line 457 in lz-string.js


          pure $ dictSize - 1

        -- line 469
        dictionary <- readRef dictionaryRef
        let dictSize = length dictionary
        w <- readRef wRef
        let
          entry =
            if c == dictSize
            then w :|> (unsafeHead w)
            else
              case dictionary Seq.!? c of
                Just val -> val
                Nothing -> error "return null"

        modifyRef dictionaryRef $
          (:|> (w :|> unsafeHead entry))

        writeRef wRef entry
        modifyRef bitCounterRef tick

        tell $ toBytes entry

toBytes :: Seq Char -> Builder
toBytes = L.stringUtf8 . toList


tick :: BitCounter -> BitCounter
tick (BitCounter e n) = do
  let e' = e - 1
  if e' > 0
  then BitCounter e' n
  else BitCounter (1 `shiftL` n) (n + 1)

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
