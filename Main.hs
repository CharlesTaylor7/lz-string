module Main where

import System.IO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64


main :: IO ()
main = do
  handle <- openFile "../generals-bot/replays/rtQyMFIwv.gior" ReadMode
  contents <- BS.hGetContents handle

  print $ BS.take 3 $ Base64.encode contents
