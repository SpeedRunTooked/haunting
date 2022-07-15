{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Codec.Ghost where

import qualified Data.Binary.Get as G

import qualified Data.Vector as V
import           Data.Vector (Vector)

import qualified Data.ByteString.Lazy as BS
import           Data.ByteString.Lazy (ByteString)
import           Data.Text.Lazy (Text)
import           Data.String ( IsString )

import           Data.ByteString.Lazy.Base64 (encodeBase64)

import           GHC.Int (Int64)
import           GHC.Word

import Data.Aeson

import Data.Ghost

import Codec.MIO0 (decodeMIO)

import Debug.Trace (trace)

data Raw
data Parsed
data Processed

type family Slot a where
  Slot Raw       = RawSlot
  Slot Parsed    = GhostMIO
  Slot Processed = Ghost

type Racer = Word8
type Track = Word8

data GhostHeader = Empty
                 | GH { character :: Racer
                      , track     :: Track
                      , raw       :: ByteString
                      }

data GhostFile a = GF { slot1 :: Slot a
                      , slot2 :: Slot a
                      }

data RawSlot = RS { rawHeader :: ByteString
                  , rawBody   :: ByteString
                  }

data GhostMIO = GM { parsedHeader :: GhostHeader
                   , parsedBody   :: ByteString
                   }

data Ghost = G { header :: GhostHeader
               , body   :: V.Vector GhostEntry
               }

mkRawGhost :: ByteString -> GhostFile Raw
mkRawGhost bytes = GF (getRawSlot 1 bytes) (getRawSlot 2 bytes)

calcOffset :: Int64 -> Int64
calcOffset i = (i - 1) * 15360 + 256

getRawSlot :: Int64 -> ByteString -> RawSlot
getRawSlot i bytes = RS head bod
 where
   head = BS.take 128 (BS.drop (i - 1) bytes)
   bod  = BS.take (60 * 256) (BS.drop (calcOffset i) bytes)

parseRaw :: GhostFile Raw -> GhostFile Parsed
parseRaw (GF (RS h1 b1) (RS h2 b2)) = GF (GM (parse h1) b1) (GM (parse h2) b2)
 where
  parse bs = G.runGet (getHeader bs) bs

  getHeader bs =
    do used <- G.skip 4 >> G.getWord8
       if used == 0x0
       then pure Empty
       else do trck <- G.getWord8
               racr <- G.getWord8
               pure (GH racr trck bs)

processParsed :: GhostFile Parsed -> Either String (GhostFile Processed)
processParsed (GF (GM h1 b1) (GM h2 b2)) =
 do b1' <- convertGhost <$> decodeMIO b1
    b2' <- convertGhost <$> decodeMIO b2
    pure (GF (G h1 b1') (G h2 b2'))

instance ToJSON (GhostFile Parsed) where
  toJSON (GF s1 s2) = object ["slot1" .= toJSON s1, "slot2" .= toJSON s2]

instance ToJSON GhostMIO where
  toJSON (GM h b) = object ["header" .= toJSON h, "ghost" .= toJSON (encodeBase64 b)]

instance ToJSON GhostHeader where
  toJSON Empty        = object []
  toJSON (GH c t raw) = object ["character" .= toJSON (chars c :: Text)
                               ,"track"     .= toJSON (tracks t :: Text)
                               ,"raw"       .= toJSON (encodeBase64 raw)
                               ]

chars :: IsString s => Word8 -> s
chars i = ["Mario", "Luigi", "Yoshi", "Toad"
          ,"D.K.", "Wario", "Peach", "Bowser"] !! (fromIntegral i)

tracks :: IsString s => Word8 -> s
tracks t = 
  ["Luigi Raceway" ,"Moo Moo Farm" ,"Koopa Troopa Beach" ,"Kalimari Desert"
  ,"Toad's Turnpike" ,"Frappe Snowland" ,"Choco Mountain" ,"Mario Raceway"
  ,"Wario Stadium" ,"Sherbet Land" ,"Royal Raceway" ,"Bowser's Castle"
  ,"DK's Jungle Parkway" ,"Yoshi Valley" ,"Banshee Boardwalk" ,"Rainbow Road"
  ] !! (fromIntegral t)
