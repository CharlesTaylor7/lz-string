module Main where

import Criterion.Main

import System.IO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64

import qualified LZString as LZ

main :: IO ()
main = do
  handle <- openFile "../generals-bot/replays/rtQyMFIwv.gior" ReadMode
  contents <- Base64.encode <$> BS.hGetContents handle

  let decompressed = LZ.decompressBase64 contents

  print $ BS.length contents
  print $ length decompressed
  print $ decompressed `seq` ()
