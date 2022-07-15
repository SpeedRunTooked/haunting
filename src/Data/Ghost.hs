module Data.Ghost where

import GHC.Word
import GHC.Int
import Data.Bits ((.&.))


import qualified Data.Vector as V
import           Data.Vector (Vector)

import qualified Data.Vector.Unboxed as VU

-- Button State:
--
-- 0x80 = A
-- 0x40 = B
-- 0x20 = Z
-- 0x10 = R
data ButtonState = BS { a :: Bool
                      , b :: Bool
                      , z :: Bool
                      , r :: Bool
                      }
  deriving Show


data GhostEntry = GE { buttState  :: ButtonState
                     , frameCount :: Word8
                     , aStickY    :: Int8
                     , aStickX    :: Int8
                     }
  deriving Show

-- | Given the controller state stream, create a stream of
--   GhostEntries
convertGhost :: VU.Vector Word8 -> Vector GhostEntry
convertGhost vec
  | VU.length vec `mod` 4 /= 0 = error "huh"
  | otherwise                 = V.convert (V.generate (VU.length vec `div` 4) f)
 where
  f i = mkGhostEntry (VU.slice (i * 4) 4 vec)


-- | Given a vector of length 4, produce a GhostEntry
mkGhostEntry :: VU.Vector Word8 -> GhostEntry
mkGhostEntry v
  | VU.length v < 4 = error "This shouldn't happen, Jose fucked up."
  | otherwise      = GE (mkButtonState (VU.head v))
                        (v VU.! 1)
                        (fromIntegral (v VU.! 2))
                        (fromIntegral (v VU.! 3))


mkButtonState :: Word8 -> ButtonState
mkButtonState byte = BS a b z r
 where
  a = (byte .&. 0x80) == 0x80
  b = (byte .&. 0x40) == 0x40
  z = (byte .&. 0x20) == 0x20
  r = (byte .&. 0x10) == 0x10
