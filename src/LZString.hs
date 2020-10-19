{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module LZString (decompressBase64) where

import Data.Function ((&), on)
import Data.Traversable (for)
import Data.Word (Word8)
import Data.Bits (Bits(..))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Control.Applicative (liftA2)
import Control.Monad (when)

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


while :: m Bool -> m ()
while act = do
  b <- act
  when b $
    while act


{-# NOINLINE _decompress #-}
_decompress :: (Length, ResetValue, GetNextValue) -> ByteString
_decompress (length, resetValue, getNextValue) = unsafePerformIO $ do
  dictionaryRef <- newIORef (mempty :: IntMap ByteString)
  nextRef <- newIORef (0 :: Int)
  enlargeInRef <- newIORef (4 :: Int)
  dictSizeRef <- newIORef (4 :: Int)
  numBitsRef <- newIORef (3 :: Int)
  iRef <- newIORef (0 :: Int)
  bitsRef <- newIORef (0 :: Int)
  resbRef <- newIORef (0 :: Int)
  maxpowerRef <- newIORef (0 :: Int)
  powerRef <- newIORef (0 :: Int)
  cRef <- newIORef (0 :: Int)
  entryRef <- newIORef ("" :: ByteString)
  wRef <- newIORef (Nothing :: Maybe ByteString)
  resultRef <- newIORef ("" :: ByteString)
  data_val <- newIORef $ getNextValue 0
  data_position <- newIORef $ resetValue
  data_index <- newIORef $ 1

  for [0..2] $ \i -> do
    modifyIORef' dictionary $
      Map.insert i $ BS.singleton (toEnum i)

  writeIORef maxpower 4
  writeIORef power 1

  let
    powersNotEqual :: IO Bool
    powersNotEqual = do
      p <- readIOref power
      m <- readIORef maxpower
      pure $ m /= p

  while powersNotEqual $ do
    val <- readIORef data_val
    pos <- readIORef data_position
    writeIORef resb $ val .&. pos

    let pos' = pos `shiftR` 1
    writeIORef data_position pos'
    when (pos' == 0) $ do
      writeIORef data_position resetValue

      -- these three lines are 1 line in the original source
      -- assignment with postfix increment is pretty complex
      index <- readIORef data_index
      writeIORef data_val $ getNextValue index
      writeIORef data_index $ index + 1

    resb' <-
    modifyIORef bits $ (.|.)

  pure undefined



