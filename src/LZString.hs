{-# LANGUAGE OverloadedStrings #-}
module LZString (decompressBase64) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

-- Use an Intmap to mimic a js array
import Data.IntMap (IntMap)
import qualified Data.IntMap as Map


keyStrBase64 :: ByteString
keyStrBase64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=";

f = fromCharCode
-- | from a list of character code points to a utf-16 encoded bytestring
fromCharCode :: [Int] -> ByteString
fromCharCode = undefined


getBaseValue :: alphabet -> character -> Int
getBaseValue = undefined


decompressBase64 :: ByteString -> ByteString
decompressBase64 = undefined

_decompress :: (Int, Int, Int -> Int) -> IO ByteString
_decompress (length, resetValue, getNextValue) = undefined


