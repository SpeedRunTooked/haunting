module Main where

import Data.Pak
import Data.Pak.Parse 
import Data.Pak.INodeTable 
import Codec.MIO0 (decodeMIO)
import qualified Data.Vector.Unboxed as V
import qualified Data.ByteString.Lazy as BS
import Numeric as N

import Data.Ghost
import Data.Ghost.CSV

import System.Environment (getArgs)

import Data.Ghost

main :: IO ()
main = do
  [f] <- getArgs
  Right toad <- parsePak f
  let Right (Pak _ itab _ rp)  = processPak toad
      Just toadbytes  = getPages (fileSeq (iNodeFiles itab !! 0)) rp
      decoded = decodeMIO (BS.drop 256 toadbytes)
  case (fmap convertGhost decoded) of
    Left err -> print err
    Right gs -> BS.putStr (ghostToCSV gs)
