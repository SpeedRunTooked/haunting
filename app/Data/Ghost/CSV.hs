{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Ghost.CSV where

import GHC.Int

import GHC.Generics (Generic)

import qualified Data.ByteString.Lazy as BS

import qualified Data.Vector as V
import           Data.Vector (Vector)

import Data.Ghost as G

import Data.Csv

data GRecord = R { a_status :: Int8
                 , b_status :: Int8
                 , z_status :: Int8
                 , r_status :: Int8
                 , analog_y :: Int8
                 , analog_x :: Int8
                 }
  deriving (Generic)

instance FromRecord GRecord
instance ToRecord GRecord
instance FromNamedRecord GRecord
instance ToNamedRecord GRecord

ghostToCSV :: V.Vector GhostEntry -> BS.ByteString
ghostToCSV = encodeByName header . ghostToRecord
 where
  header = V.fromList ["a_status"
                      ,"b_status"
                      ,"z_status"
                      ,"r_status"
                      ,"analog_y"
                      ,"analog_x"
                      ]

ghostToRecord :: V.Vector GhostEntry -> [GRecord]
ghostToRecord vec =
  concat [replicate (fromIntegral count) (R a' b' z' r' asy asx)
         | G.GE (G.BS a b z r) count asy asx <- V.toList vec
         , let [a',b',z',r'] = map (fromIntegral . fromEnum) [a,b,z,r]
         ]


