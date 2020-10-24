{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module LZString (decompressBase64) where

import Prelude hiding (break, print, putStrLn)

import GHC.Exts
import Data.Function ((&))
import Data.Foldable (foldlM, length)
import Data.Word (Word8)
import Data.Bits (Bits(..))
import Data.IORef (IORef)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (MonadState, get, gets, put, evalStateT)
import Control.Monad.Writer.Strict (execWriterT, tell)

import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Text.Lazy.Builder as TextBuilder

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Data.IntMap (IntMap)
import qualified Data.IntMap as Map

import Data.Array (Array)
import qualified Data.Array as Array

import Utils

type TextBuilder = TextBuilder.Builder

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

f :: Int -> String
f = pure . toEnum

_decompressImpl :: (Length, ResetValue, GetNextValue) -> IO Decompressed
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
    c <- getBits' exponent
    let
      w = f c

    -- refs
    dictionaryRef <- newRef $
      (fromList [ f 0, f 1, f 2, w] :: Seq String)
    enlargeInRef <- newRef (4 :: Int)
    numBitsRef <- newRef (3 :: Int)
    wRef <- newRef (w :: Decompressed)
    cRef <- newRef c

    -- loop
    result <- execWriterT $ loop $ do
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
        modifyRef dictionaryRef $
          (Seq.|> (f bits))

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
              pure $ w <> [(w !! 0)]
            else
              error "return null"

      tell entry

      modifyRef dictionaryRef $
        (Seq.|> (w <> [entry !! 0]))

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
