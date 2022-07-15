module Main where

import Data.Pak hiding (Parsed)
import Data.Pak.Parse 
import Data.Pak.INodeTable 
import Codec.MIO0 (decodeMIO)
import qualified Data.Vector.Unboxed as V
import qualified Data.ByteString.Lazy as BS
import Numeric as N

import Data.Aeson (encode)

import           GHC.Int (Int64)
import qualified System.IO            as IO
import           System.IO (stdin, stdout, stderr)

import Main.Options

import Data.Ghost
import Codec.Ghost
import Data.Ghost.CSV


main :: IO ()
main = do
  opts      <- getOpts
  outHandle <- mkHandle opts
  case opts of
    Opt mode (FPath f) _ -> do
      processed <- fmap (>>= processPak) (parsePak f)
      case processed of
        Left err -> error "Couln't process pak file"
        Right (Pak _ itab _ rp) ->
          case getPages (fileSeq (iNodeFiles itab !! 0)) rp of
            Nothing    -> error "TODO: Make a better error message"
            Just bytes -> doDump outHandle mode bytes
    Opt _ f _ -> error "TODO: We do not yet support input from stdin"


doDump h (DumpCSV s) bytes =
      let processed = (processParsed . parseRaw . mkRawGhost) bytes in
      case processed of
        Left err -> error err
        Right (GF s1 s2) -> BS.hPutStr h (ghostToCSV (body (pickSlot s s1 s2)))

doDump h (DumpGhost s) bytes =
      let GF s1 s2 = (parseRaw . mkRawGhost) bytes in
      BS.hPutStr h (encode (pickSlot s s1 s2))
      -- case processed of
      --   Left err -> error err
      --   Right gf -> BS.hPutStr h (encode gf)

doDump h Meta bytes =
      let parsed = (parseRaw . mkRawGhost) bytes in
      IO.hPutStr h (displaySlotHeaders parsed)

displaySlotHeaders :: GhostFile Parsed -> String
displaySlotHeaders (GF s1 s2) = "Slot 1:\n"
                             ++ "\t" ++ displayGH (parsedHeader s1) ++ "\n"
                             ++ "\nSlot 2:\n"
                             ++ "\t" ++ displayGH (parsedHeader s2) ++ "\n\n"

displayGH :: GhostHeader -> String
displayGH Empty      = "No ghost data in this slot"
displayGH (GH c t _) = "Driver: " ++ chars c ++ "\n\tTrack: " ++ tracks t

pickSlot :: Int -> a -> a -> a
pickSlot s s1 s2
  | s == 1 = s1
  | s == 2 = s2

mkHandle (Opt _ _ Stdout)     = pure stdout
mkHandle (Opt _ _ (OFPath f)) = IO.openFile f IO.WriteMode
