module Main where

import Data.Pak
import Data.Pak.Parse 
import Data.Pak.INodeTable 
import Codec.MIO0 (decodeMIO)
import qualified Data.Vector.Unboxed as V
import qualified Data.ByteString.Lazy as BS
import Numeric as N

import Data.Ghost

import System.Environment (getArgs)

import Data.Ghost

main :: IO ()
main = do
  [f] <- getArgs
  Right toad <- parsePak f
  let Right (Pak _ itab _ rp)  = processPak toad
      Just toadbytes  = getPages (fileSeq (iNodeFiles itab !! 0)) rp
      decoded = decodeMIO (BS.drop 256 toadbytes)
  print decoded
